program fpctwit;

{ Application that demonstrates use of the Twitter API with FreePascal.

  Copyright (c) 2012 Reinier Olislagers

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF}
  {Needed for Widestring/UTF8 support}
  cwstring, {$ENDIF}
  {$IFDEF WINDOWS}
  Windows, {for setconsoleoutputcp}
  ctypes,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Keyboard,
  inifiles,
  twitter, oauth1;
// Enable this if you want to test using the OAuth1Test object which
// allows you to test signature calculation etc with some sample data
{not $DEFINE COMPLETEOAUTH1TEST}
type

  { Tfpctwit }

  Tfpctwit = class(TCustomApplication)
  private
    {$IFDEF WINDOWS}
    FCurrentOutputCP: cuint;
    {$ENDIF}
    FReceiveBufferTweetStream: string;
    FStreamLog: Text; //Output of streaming demo
    FTwitter: TTwitter; //Our Twitter connection
    procedure WriteAndClearTweets(Tweets: TTweetsArray);
  protected
    function ReceiveHandlerTweetStream(ReceiveText: string): boolean;
    procedure ConnectedTests; //Tests that only make sense if connected to Twitter
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  function RandomNumbers: string;
    // Generate some characters of random numbers
  const
    TextLength = 5;
    ValidChars = '0123456789';
  var
    i: integer;
  begin
    Result := StringOfChar('q', TextLength);
    for i := 1 to TextLength do
    begin
      Result[i] := ValidChars[Trunc((Length(ValidChars) * Random) + 1)];
    end;
  end;


  function GetPIN(URL: string): string;
    // Callback function: called when a PIN is requested by the authentication library
    // We get a URL the user needs to go to, and we should return the PIN the user
    // got from that URL.
  var
    PIN: string;
  begin
    writeln('Please go to: (note: URL should be all on one line!!!)');
    //see e.g.
    //https://dev.twitter.com/docs/api/1/get/oauth/authorize
    writeln(URL);
    writeln('... and copy the PIN you receive.');
    repeat
      begin
        writeln('Please enter the PIN:');
        readln(PIN);
      end;
    until PIN <> '';
    Result := PIN;
  end;

  { Tfpctwit }

  procedure Tfpctwit.WriteAndClearTweets(Tweets: TTweetsArray);
  var
    i: integer;
  begin
    for i := 0 to Length(Tweets) - 1 do
    begin
      writeln('Tweet: ');
      //Note: Twitter class uses UTF8 strings; console should be set up for this
      //otherwise convert using UTF8ToAnsi
      writeln(Tweets[i].User + ' (' + DateTimeToStr(Tweets[i].Timestamp) + ' UTC' + '): ');
      writeln(Tweets[i].Message);
      writeln('');
    end;
    Tweets := nil;
  end;

  function Tfpctwit.ReceiveHandlerTweetStream(ReceiveText: string): boolean;
    // Example handler that shows how to output incoming tweets to screen. Of course,
    // requires console application.
  var
    i: integer;
    K: TKeyEvent;
    LineEnd: integer;
    Tweets: TTweetsArray;
  begin
    // Break lines on #13#10
    LineEnd := pos(#13 + #10, ReceiveText);
    if LineEnd = 0 then
    begin
      FReceiveBufferTweetStream := FReceiveBufferTweetStream + ReceiveText;
    end
    else
    begin
      Tweets := FTwitter.ProcessTweets(FReceiveBufferTweetStream + Copy(ReceiveText, 1, LineEnd - 1));
      // LineEnd+2 because we should always expect CR+LF according to Twitter specs.
      FReceiveBufferTweetStream := Copy(ReceiveText, LineEnd + 2, Length(ReceiveText));
      for i := 0 to Length(Tweets) - 1 do
      begin
        // Let's output to screen for now; that's the only feedback a user will get that
        // things are working.
        // An increasing timestamp difference suggests we can't keep up with the stream.
        writeln('Behind:    ' + TimeToStr(FTwitter.GetUTCTime - Tweets[i].TimeStamp));
        writeln('ID:        ' + IntToStr(Tweets[i].ID));
        if Tweets[i].Deleted then
          writeln('********** DELETED *****************');
        writeln('When (UTC):' + DateTimeToStr(Tweets[i].Timestamp));
        //Twitter class uses UTF8 strings - but we set up our console to support that
        writeln('User (ID): ' + Tweets[i].User + IntToStr(Tweets[i].UserID));
        writeln('Tweet:     ' + Tweets[i].Message);
        writeln('');
        // Output to file, too: (note: not using UTF8ToAnsi and hoping the strings will come through unscathed)
        writeln(FStreamLog, IntToStr(Tweets[i].ID) + #9 + Tweets[i].User + #9 + Tweets[i].Message + #9 +
          DateTimeToStr(Tweets[i].Timestamp));
      end;
    end;
    Result := true;
    K := PollKeyEvent; //See whether user pressed any key
    if K <> 0 then
    begin
      K := GetKeyEvent;
      K := TranslateKeyEvent(K);
      if GetKeyEventChar(K) = 'q' then
      begin
        // User wants to stop
        writeln('User pressed q. Stopping.');
        Result := false;
      end;
    end;
  end;

  procedure Tfpctwit.ConnectedTests;
  var
    ResultCode: integer;
    ReturnBody: string;
    TestTweet: string;
    Tweets: TTweetsArray;
  {$IFDEF COMPLETEOAUTH1TEST}
    TwitterTest: OAuth1Test;
  {$ENDIF COMLETEOAUTH1TEST}
    URL: string;
  begin
    try
      writeln('');
      // Alea iacta est in Greek... and goodbye in Russian
      TestTweet := 'test tweet ἀνεῤῥίφθω κύβος ' + RandomNumbers + ' до свидания';
      writeln('Tweeting this "' + TestTweet + '" as test.');
      // We could also use just FTwitter.Tweet to send the tweet and just get a success/failure result.
      Tweets := FTwitter.TweetAndSee(TestTweet);
      if Length(Tweets) = 0 then
        writeln('Tweet failed.');
      WriteAndClearTweets(Tweets);
    except
      on E: Exception do
      begin
        writeln('Exception: ' + E.ClassName + '/' + E.Message);
        exit; //Stop program
      end;
    end;

    try
      writeln('');
      writeln('Showing your home timeline:');
      Tweets := FTwitter.GetTweets('');
      WriteAndClearTweets(Tweets);

      writeln('Showing public timeline:');
      URL := TwitterAPIURL + '/1/statuses/public_timeline.json';
      Tweets := FTwitter.GetTweets(URL);
      WriteAndClearTweets(Tweets);
    except
      on E: Exception do
      begin
        writeln('Exception: ' + E.ClassName + '/' + E.Message);
        exit; //Stop program
      end;
    end;


    //Try an URL that should not exist => test 404 error handling
    try
      URL := 'https://api.twitter.com/5/bla/does/not/exist/whatistheanswer/42.json';
      writeln('');
      writeln('Calling non-existing URL:');
      ReturnBody:='';
      ResultCode:=FTwitter.HTTPMethod('GET', URL, ReturnBody);
      if ResultCode = 200 then
      begin
        writeln('Returned body:');
        writeln(ReturnBody);
      end
      else
      begin
        writeln('Test succeeded (not found): result code: ' + IntToStr(ResultCode));
      end;
    except
      on E: Exception do
      begin
        writeln('Exception: ' + E.ClassName + '/' + E.Message);
      end;
    end;

    writeln('');
    writeln('Found rate limit of ' + IntToStr(FTwitter.RateLimitRemaining) + ' for approximately the next ' +
      TimeToStr(FTwitter.RateLimitTimeRemaing));

    // Sip from the firehose... ehm garden hose, no... spritzer.
    writeln('');
    writeln('Going to demonstrate streaming API by continuously getting and showing tweets.');
    writeln('When the tweet stream is running, you can stop it by pressing q.');
    writeln('Now, press enter to continue or q and enter to quit.');
    ReadLn(ReturnBody);
    if uppercase(trim(ReturnBody))<>'Q' then
    begin
      writeln('');
      writeln('Streaming API demo:');
      System.Assign(FStreamLog, 'stream.txt'); //Twitter stream will be saved to file
      Rewrite(FStreamLog); //use this for output of debug messages etc.

      FReceiveBufferTweetStream := ''; //Buffer must be clean before ReceiveHanlderTweetStream being called
      InitKeyBoard; //Set up keyboard unit so we can catch q in the loop
      FTwitter.GetTweetStream(@ReceiveHandlerTweetStream);
      try
        Close(FStreamLog);
      except
        // Closing log file failed. Too bad, ignore it.
      end;
    end;

    writeln('');
    writeln('Logging out of session:');
    FTwitter.Disconnect;
    writeln('Logout procedure completed.');

    writeln('');

  end;

  procedure Tfpctwit.DoRun;

  const
    SecretsFile = 'twitterauthcodes.ini';
  var
    AccessToken: string;
    AccessTokenSecret: string;
    ConsumerKey: string;
    ConsumerSecret: string;
    ErrorMsg: string;
    Tweets: TTweetsArray;
    TwitterINI: TINIFile;
    i: integer;
  {$IFDEF COMPLETEOAUTH1TEST}
    TwitterTest: OAuth1Test;
  {$ENDIF COMLETEOAUTH1TEST}
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if FileExists(SecretsFile) then
    begin
      TwitterINI := TINIFile.Create(SecretsFile);
      try
        ConsumerKey := TwitterINI.ReadString('Secrets', 'ConsumerKey', '');
        ConsumerSecret := TwitterINI.ReadString('Secrets', 'ConsumerSecret', '');
        AccessToken := TwitterINI.ReadString('Secrets', 'AccessToken', '');
        AccessTokenSecret := TwitterINI.ReadString('Secrets', 'AccessTokenSecret', '');

        //Proxy settings if any
        FTwitter.ProxyHost := TwitterINI.ReadString('Proxy', 'Host', '');
        FTwitter.ProxyPort := TwitterINI.ReadString('Proxy', 'Port', '');
        FTwitter.ProxyUser := TwitterINI.ReadString('Proxy', 'User', '');
        FTwitter.ProxyPass := TwitterINI.ReadString('Proxy', 'Password', '');
      finally
        TwitterINI.Free;
      end;
    end;

    if ConsumerKey = '' then
    begin
      if ParamCount > 1 then
      begin
        // Assume consumer key is first, then consumer secret
        // then access token, then access token secret
        ConsumerKey := Params[1];
        ConsumerSecret := Params[2];
        AccessToken := Params[3];
        AccessTokenSecret := Params[4];
      end
      else
      begin
        writeln('Please enter your consumer key');
        readln(ConsumerKey);
        writeln('Please enter your consumer secret');
        readln(ConsumerSecret);
      end;
    end;

  {$IFDEF COMPLETEOAUTH1TEST}
    writeln('');
    writeln('====================================');
    writeln('Beginning OAuth1Test');
    TwitterTest := OAuth1Test.Create;
    try
      // todo: more testing?
    finally
      TwitterTest.Free;
    end;
    writeln('End of OAuth1Test');
    writeln('====================================');
    writeln('');
  {$ENDIF}

    // Set up
    try
      FTwitter.GetPINFunction := @GetPIN; //Register PIN callback with object
      FTwitter.ConsumerKey := ConsumerKey;
      FTwitter.ConsumerSecret := ConsumerSecret;
      // Next two will be empty if not preauthorized and using PIN auth
      FTwitter.AuthToken := AccessToken;
      FTwitter.AuthSecret := AccessTokenSecret;
      if FTwitter.Connect = false then
        writeln('Connection attempt failed.'); //Connect and verify credentials
      // Not necessary to explicitly call connect, but you may be able to deal better with credential problems here.
    except
      on E: Exception do
      begin
        writeln('Exception: ' + E.ClassName + '/' + E.Message);
        exit;
      end;
    end;

    // Let user know their access token/secret so he can save it for next time
    if (AccessToken = '') and (AccessTokenSecret = '') then
    begin
      writeln('Your appliction is authenticated; you can try to use these codes next time:');
      writeln('Access token:');
      writeln(FTwitter.AuthToken);
      writeln('Access secret:');
      writeln(FTwitter.AuthSecret);
    end;

    writeln('Testing Twitter library processing of a single sample tweet:');
    // modified real tweet
    Tweets := FTwitter.ProcessTweets(
      '{"entities":{"urls":[{"display_url":"goo.l\/fb\/QD3eg","indices":[30,50],"expanded_url":"http:\/\/goo.l\/fb\/QD3eg","url":"http:\/\/t.co\/PuOk5RJZ"}],"user_mentions":[],"hashtags":[]},"contributors":null,"retweet_count":0,"text":"gapzs netent  http:\/\/t.co\/PuOk6RJZ","in_reply_to_user_id_str":null,"place":null,"in_reply_to_status_id":null,"in_reply_to_user_id":null,"truncated":false,"coordinates":null,"geo":null,"retweeted":false,"source":"\u003Ca href=\"http:\/\/www.google.com\/\" rel=\"nofollow\"\u003EGoogle\u003C\/a\u003E","possibly_sensitive_editable":true,"id_str":"215774513877614591","created_at":"Thu Jun 21 11:52:44 +0000 2012","possibly_sensitive":false,"in_reply_to_screen_name":null,"user":{"listed_count":0,"lang":"en","profile_background_tile":false,"location":"","time_zone":null,"profile_sidebar_fill_color":"DDEEF6","profile_image_url_https":"https:\/\/si0.twimg.com\/profile_images\/1694260064\/tu_4868a0den__77b0f2_normal.jpg","default_profile":true,"is_translator":false,"contributors_enabled":false,"following":null,"geo_enabled":false,"profile_sidebar_border_color":"C0DEED","description":"din \u0111\u00e0n ca s\u1ebb inh ghi\u1ec7m","follow_request_sent":null,"verified":false,"profile_use_background_image":true,"id_str":"437295489","default_profile_image":false,"show_all_inline_media":false,"profile_text_color":"333333","profile_background_image_url_https":"https:\/\/si0.twimg.com\/images\/themes\/theme1\/bg.png","profile_background_image_url":"http:\/\/a0.twimg.com\/images\/themes\/theme1\/bg.png","favourites_count":0,"created_at":"Thu Dec 15 09:18:55 +0000 2011","friends_count":12,"protected":false,"profile_link_color":"0084B4","followers_count":20,"url":"http:\/\/cacm.vn","profile_image_url":"http:\/\/a0.twimg.com\/profile_images\/1694261064\/tu_4868a0den__77b0f2_normal.jpg","screen_name":"caembo","name":"caembo","notifications":null,"id":437295489,"statuses_count":2580,"utc_offset":null,"profile_background_color":"C0DEED"},"favorited":false,"id":215774513877614591,"in_reply_to_status_id_str":null}');
    for i := 0 to Length(Tweets) - 1 do
    begin
      writeln('Tweet: ');
      writeln(Tweets[i].User + ' (' + DateTimeToStr(Tweets[i].Timestamp) + '): ');
      writeln(Tweets[i].Message);
      writeln('');
    end;
    Tweets := nil;

    if FTwitter.Connected then
    begin
      ConnectedTests;
    end
    else
    begin
      writeln('Not connected, cannot run further tests, sorry.');
    end;

    // stop program loop
    Terminate;
  end;

  constructor Tfpctwit.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := true;
    // Set up console for Windows
    {$IFDEF WINDOWS}
    // Set up UTF8 output; see
    //http://www.lazarus.freepascal.org/index.php/topic,13949.msg74232.html#msg74232
    FCurrentOutputCP:=GetConsoleOutputCP;
    SetConsoleOutputCP(CP_UTF8);
    {$ENDIF}
    FTwitter := TTwitter.Create;
  end;

  destructor Tfpctwit.Destroy;
  begin
    FTwitter.Free;
    {$IFDEF WINDOWS}
    // reset console
    SetConsoleOutputCP(FCurrentOutputCP);
    {$ENDIF}
    inherited Destroy;
  end;

  procedure Tfpctwit.WriteHelp;
  begin
    writeln('fpctwit: FreePascal code for Twitter API');
    writeln('Demonstrates authentication, getting tweets and tweeting.');
    writeln('Usage: ', ExeName, ' -h');
    writeln('');
    writeln('Options to specify your keys/settings:');
    writeln('1. Save your twitter ConsumerKey, ConsumerSecret (and ');
    writeln('   AccessToken, AccessTokenSecret if you have them)');
    writeln('   in twitterauthcodes.ini, e.g.:');
    writeln('[Secrets]');
    writeln('ConsumerKey=<replace with your key of course>');
    writeln('ConsumerSecret=<replace with your secret of course>');
    writeln('AccessToken=<replace with your key of course>');
    writeln('AccessTokenSecret=<replace with your secret of course>');
    writeln('');
    writeln('Note: you can write a [Proxy] section with Host, Port,');
    writeln('User and Password if you want to use an HTTP/HTTPS proxy.');
    writeln('');
    writeln('2. Specify them as arguments on the command line or');
    writeln('3. Let the program ask for them');
    writeln('');
  end;

var
  Application: Tfpctwit;
begin
  Application := Tfpctwit.Create(nil);
  Application.Run;
  Application.Free;
end.
