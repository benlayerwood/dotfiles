Config {font = "xft:Roboto:size=12:style=Semibold"
       , additionalFonts = [
                             "xft:Source Sans Pro:size=12:weight=Semibold"
                           , "xft:Inconsolata Medium:pixelsize=22"
                           , "xft:Symbols Nerd Font:pixelsize=20:style=Regular"
                           , "xft:Fira Code:pixelsize=18"
                           , "xft:Roboto:pixelsize=14:style=Bold"
                           , "xft:Noto Sans Mono:pixelsize=14:style=Bold"
                           , "xft:Free Serif:pixelsize=20:style=Regular"
                           ]
       , border = TopB
       , borderWidth = 0
       , borderColor = "#212121"
       , bgColor = "#212121"
    , fgColor = "white"
    , alpha = 255
       , position = TopSize C 100 30
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = True
       , hideOnStart = False
       , iconRoot = "/home/ben/.config/xmobar/icons/"
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ 
         Run Network "wlan0" ["--template" , "<dev>: <rx>KB","-L","0","-H","32","--normal","green","--high","red"] 10,
--       alias is wlan0wi
         Run Wireless "wlan0" ["--template"
                              ,"<fc=#bbfe9f><fn=3></fn></fc>  <ssid> <quality>% "
                              ,"--Low", "30"
                              ,"--High", "70"
                              ,"--low", "bbfe9f"
                              ,"--high", "ff0000"
                              ] 100,
			Run Memory ["-t","<fc=#cb94e6><fn=3></fn></fc> <usedratio>%"] 10,
			Run Swap [] 10,
			Run Com "uname" ["-s","-r"] "" 36000,
			Run Com "whoami" [] "" 36000,
			Run Com "hostname" [] "" 36000,
			Run Date "%a %d.%m.%Y %H:%M" "date" 10,
			Run Volume "default" "Master" ["--template" 
                              , "<volume>%" 
                              ] 10,
         Run Locks,
         Run DiskU [
               ("/home", "<fc=#9593fd><fn=3></fn></fc> <usedp>%"),
               ("/","<fc=#9593fd><fn=3></fn></fc> <usedp>%")] [] 60,
			Run Battery     [ "--template" , "<acstatus>"
                             , "--Low"      , "40"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--" -- battery specific options
                                       , "-L"    , "25"
                                       , "-H"    , "75"
                                       , "-o"   , " <left>%"
                                       , "-O"	, "<fn=3></fn>  <fn=3></fn>"
                                       , "-i"	, "<fn=3></fn>  <fn=3></fn>"
                                       , "--lows", "<fc=#ff413a><fn=3></fn></fc>"
                                       , "--mediums", "<fc=#eba428><fn=3></fn></fc>"
                                       , "--highs", "<fc=#49d23b><fn=3></fn></fc>"
                             ] 50,
          Run WeatherX "EDSB"
             [ ("clear", "")
             , ("sunny", "")
             , ("", "")
             , ("mostly clear", "")
             , ("mostly sunny", "")
             , ("partly sunny", "")
             , ("fair", "🌑")
             , ("cloudy","")
             , ("overcast","")
             , ("partly cloudy", "")
             , ("mostly cloudy", "")
             , ("drizzle", "")
             , ("obscured", "")
             , ("light rain", "")
             , ("considerable cloudiness", "")]
             ["-t", "<fn=3><skyConditionS></fn> <tempC>°"
             , "-L","10", "-H", "25", "--normal", "lightgreen"
             , "--high", "lightgoldenrod4", "--low", "lightblue"]
             18000,
            --https://tgftp.nws.noaa.gov/weather/current/EDSB.html
           Run Kbd            [ ("us(dvorak)" , "<fc=#00008B>DV</fc>")
                             , ("us"         , "<fc=#8B0000>US</fc>")
                             ],
           Run Uptime [ "--template", "<fn=3></fn> <hours>:<minutes>"
                      , "-S", "true" 
                      ] 10,
         Run UnsafeStdinReader
        ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %UnsafeStdinReader%\
                    \}\
                    \%EDSB%\
                    \<fc=#3b3b3b,#212121><fn=4></fn></fc><fn=5>\
                    \<fc=#46dbf1,#3b3b3b>  %date%  </fc></fn>\
                    \<fc=#3b3b3b,#212121><fn=4></fn></fc> \
                    \%uptime%\
                    \{\
                    \%locks%   %memory%  %disku%  %battery%\
                    \                      \
                    \                      \
                    \                 "
