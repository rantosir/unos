#
# Marky's Uno v0.96
# Copyright (C) 2004 Mark A. Day (techwhiz@earthlink.net)
# Copyright (C) 2005 SPiRiTX for Modifying :P (spiritx@freaxnet.de)
#
# Uno(tm) is Copyright (C) 2001 Mattel, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

# Default Settings
set UnoChan 		"#Unopoint"
set UnoRobot 		"BotUno"
set UnoPointsName 	"Points"
set UnoStopAfter 	2
set UnoBonus		1000
set UnoWildDrawTwos	1
set UnoCFGFile		"scripts/uno.cfg"
set UnoScoreFile 	"scores/UnoScores"
set UnoMaxNickLen	9
set UnoMaxPlayers	10
set UnoOpFlags		"o|o"
set UnoNTC		"NOTICE"

# Command Binds
bind pub - !unocmds UnoCmds
bind pub - !remove UnoRemove
bind pub - !pause UnoPause
bind pub - !unowon UnoWon
bind pub - !unotop10 UnoTopTen
bind pub - !unotop10won UnoTopTenWon
bind pub - !unotop3last UnoTopThreeLast
bind pub - !unofast UnoTopFast
bind pub - !unohigh UnoHighScore
bind pub - !unoplayed UnoPlayed
bind pub - !unorecords UnoRecords
bind pub - !unoversion UnoVersion
bind pub - !uno UnoInit
bind pub - !stop UnoStop

# DCC Binds
bind dcc - unohands dccUnoHands
bind dcc - unorehash dcc_unorehash

# Cron Bind For Score Reset
bind time - "00 00 01 * *" UnoNewMonth

# Global Variables
set UnoOn 0
set UnoMode 0
set UnoPaused 0
set UnoPlayers 0
set MasterDeck ""
set UnoDeck ""
set DiscardPile ""
set PlayCard ""
set RoundRobin ""
set ThisPlayer ""
set ThisPlayerIDX 0
set UnoStartTime [unixtime]
set IsColorChange 0
set ColorPicker ""
set IsDraw 0
set UnoIDX ""
set UnPlayedRounds 0

# Scores Records And Ads
set UnoLastMonthCards(0) "Nobody 0"
set UnoLastMonthCards(1) "Nobody 0"
set UnoLastMonthCards(2) "Nobody 0"
set UnoLastMonthGames(0) "Nobody 0"
set UnoLastMonthGames(1) "Nobody 0"
set UnoLastMonthGames(2) "Nobody 0"
set UnoFast "Nobody 600"
set UnoHigh "Nobody 0"
set UnoPlayed "Nobody 0"
set UnoRecordHigh "Nobody 0"
set UnoRecordFast "Nobody 600"
set UnoRecordCard "Nobody 0"
set UnoRecordWins "Nobody 0"
set UnoRecordPlayed "Nobody 0"
set UnoAdNumber 0

# Card Stats
set CardStats(played) 0
set CardStats(passed) 0
set CardStats(drawn) 0
set CardStats(wilds) 0
set CardStats(draws) 0
set CardStats(skips) 0
set CardStats(revs) 0

# Timers
set UnoStartTimer ""
set UnoSkipTimer ""
set UnoCycleTimer ""
set UnoBotTimer ""

# Grace periods and timeouts
# AutoSkip period can be raised but not lower than 2
set AutoSkipPeriod 2
set StartGracePeriod 30
set RobotRestartPeriod 1
set UnoCycleTime 30

# Nick colours
set UnoNickColour "06 13 03 07 12 10 04 11 09 08"

# Debugging info
set Debug 0
set UnoVersion "Final Version"
# Bind Channel Commands 
#
proc UnoBindCmds {} {
 bind pub - join JoinUno
 bind pub - jo JoinUno
 bind pub - order UnoOrder
 bind pub - od UnoOrder
 bind pub - time UnoTime
 bind pub - ti UnoTime
 bind pub - cards UnoShowCards
 bind pub - ca UnoShowCards
 bind pub - play UnoPlayCard
 bind pub - pl UnoPlayCard
 bind pub - card UnoTopCard
 bind pub - cd UnoTopCard
 bind pub - turn UnoTurn
 bind pub - tu UnoTurn
 bind pub - draw UnoDraw
 bind pub - dr UnoDraw
 bind pub - color UnoColorChange
 bind pub - co UnoColorChange
 bind pub - pass UnoPass
 bind pub - pa UnoPass
 bind pub - count UnoCount
 bind pub - ct UnoCount
 bind pub - stats UnoCardStats
 bind pub - st UnoCardStats
}

#
# Unbind Channel Commands 
#
proc UnoUnbindCmds {} {
 catch {unbind pub - join JoinUno}
 catch {unbind pub - jo JoinUno}
 catch {unbind pub - order UnoOrder}
 catch {unbind pub - od UnoOrder}
 catch {unbind pub - time UnoTime}
 catch {unbind pub - ti UnoTime}
 catch {unbind pub - cards UnoShowCards}
 catch {unbind pub - ca UnoShowCards}
 catch {unbind pub - play UnoPlayCard}
 catch {unbind pub - pl UnoPlayCard}
 catch {unbind pub - card UnoTopCard}
 catch {unbind pub - cd UnoTopCard}
 catch {unbind pub - turn UnoTurn}
 catch {unbind pub - tu UnoTurn}
 catch {unbind pub - draw UnoDraw}
 catch {unbind pub - dr UnoDraw}
 catch {unbind pub - color UnoColorChange}
 catch {unbind pub - co UnoColorChange}
 catch {unbind pub - pass UnoPass}
 catch {unbind pub - pa UnoPass}
 catch {unbind pub - count UnoCount}
 catch {unbind pub - ct UnoCount}
 catch {unbind pub - stats UnoCardStats}
 catch {unbind pub - st UnoCardStats}
}

#
# Reset Game Variables
#
proc UnoReset {} {
 global UnoOn UnoMode UnoPaused UnoPlayers RoundRobin UnoDeck ThisPlayer ThisPlayerIDX PlayCard
 global DiscardPile IsColorChange ColorPicker IsDraw UnoIDX MasterDeck CardStats
 global UnoStartTimer UnoSkipTimer UnoCycleTimer

 set UnoMode 0
 set UnoPaused 0
 set UnoPlayers 0
 set MasterDeck ""
 set UnoDeck ""
 set DiscardPile ""
 set RoundRobin ""
 set ThisPlayer ""
 set ThisPlayerIDX 0
 set PlayCard ""
 set IsColorChange 0
 set ColorPicker ""
 set IsDraw 0
 set UnoIDX ""
 set UnoAdNumber 0

 set CardStats(played) 0
 set CardStats(passed) 0
 set CardStats(drawn) 0
 set CardStats(wilds) 0
 set CardStats(draws) 0
 set CardStats(skips) 0
 set CardStats(revs) 0

 set UnoStartTimer ""
 set UnoSkipTimer ""
 set UnoCycleTimer ""

 return
}

#
# Stop a game
#
proc UnoStop {nick uhost hand chan arg} {
 global Debug UnoChan UnoOn UnoPaused UnPlayedRounds UnoStartTimer UnoSkipTimer UnoCycleTimer

 if {$chan != $UnoChan} {return}

 catch {killutimer $UnoStartTimer}
 catch {killtimer $UnoSkipTimer}
 catch {killutimer $UnoCycleTimer}

 unomsg "[unoad]\00306 Stopped by \00304\[\00312$nick!$uhost\00304\]\003"

 set UnoOn 0
 set UnoPaused 0
 set UnPlayedRounds 0

 UnoUnbindCmds
 UnoReset

 return
}

#
# First Entry
#
proc UnoInit {nick uhost hand chan arg} {
 global UnoChan UnoOn 
 if {($chan != $UnoChan)||($UnoOn > 0)} {return}
 unomsg "[unoad] \00304\[\00310$nick\!$uhost\00304\]\003"
 set UnoOn 1
 UnoBindCmds
 UnoNext
 return
}

#
# Initialize a new game
#
proc UnoNext {} {
 global UnoOn MasterDeck UnoDeck UnoMode StartGracePeriod UnoHand NickColor UnoVersion UnoStartTimer UnoSkipTimer

 if {$UnoOn == 0} {return}

 UnoReset

 set UnoMode 1

 set MasterDeck [list B0 B1 B1 B2 B2 B3 B3 B4 B4 B5 B5 B6 B6 B7 B7 B8 B8 B9 B9 BR BR BS BS BDT BDT R0 R1 R1 R2 R2 R3 R3 R4 R4 R5 R5 R6 R6 R7 R7 R8 R8 R9 R9 RR RR RS RS RDT RDT Y0 Y1 Y1 Y2 Y2 Y3 Y3 Y4 Y4 Y5 Y5 Y6 Y6 Y7 Y7 Y8 Y8 Y9 Y9 YR YR YS YS YDT YDT G0 G1 G1 G2 G2 G3 G3 G4 G4 G5 G5 G6 G6 G7 G7 G8 G8 G9 G9 GR GR GS GS GDT GDT W W W W WDF WDF WDF WDF]
 set UnoDeck ""

 binary scan [unixtime] S1 rseed
 set newrand [expr srand($rseed)]

 while {[llength $UnoDeck] != 108} {
  set pcardnum [rand [llength $MasterDeck]]
  set pcard [lindex $MasterDeck $pcardnum]
  lappend UnoDeck $pcard
  set MasterDeck [lreplace $MasterDeck $pcardnum $pcardnum]
 }

 if [info exist UnoHand] {unset UnoHand}
 if [info exist NickColor] {unset NickColor}

 unomsg "0,4 UnoPoint - People's First Choice to Play UNO "
 unomsg "[unoad]\003 \0030,6 $UnoVersion Powered By UnoPoint Team \003"
 unomsg "[unoad]\003 \0030,12 Join in $StartGracePeriod Seconds or GTH \003"

 set UnoStartTimer [utimer $StartGracePeriod UnoStart]

 return
}

#
# Start a new game
#
proc UnoStart {} {
 global UnoChan UnoOn UnoCycleTime UnoRobot Debug UnoIDX UnoStartTime UnoPlayers RoundRobin ThisPlayer ThisPlayerIDX UnoDeck DiscardPile UnoMode UnoHand PlayCard AutoSkipPeriod
 global UnoSkipTimer UnPlayedRounds UnoStopAfter NickColor

 if {$UnoOn == 0} {return}

 if {[llength $RoundRobin] == 0} {
  unomsg "[unoad] \0030,3 No Crazy Head Joined Yet - Next Game in $UnoCycleTime Seconds \003"
  incr UnPlayedRounds
  if {($UnoStopAfter > 0)&&($UnPlayedRounds >= $UnoStopAfter)} {
    unomsg "[unoad] \0030,6 Fine GTH I am Stopping Game After $UnoStopAfter Unplayed Rounds - Type !uno To Restart \003"
    utimer 1 "UnoStop $UnoRobot $UnoRobot none $UnoChan none"
    return
  }
  UnoCycle
  return
 }

 # Bot Joins If One Player

 if {[llength $RoundRobin] == 1} {
  incr UnoPlayers

  lappend RoundRobin "$UnoRobot"
  lappend UnoIDX "$UnoRobot"

  if [info exist UnoHand($UnoRobot)] {unset UnoHand($UnoRobot)}
  if [info exist NickColor($UnoRobot)] {unset NickColor($UnoRobot)}

  set UnoHand($UnoRobot) ""
  set NickColor($UnoRobot) [colornick $UnoPlayers]

  unomsg "[nikclr $UnoRobot]\003 join's [unoad]\003 table"

  UnoShuffle 7

  while {[llength $UnoHand($UnoRobot)] != 7} {
   set pcardnum [rand [llength $UnoDeck]]
   set pcard [lindex $UnoDeck $pcardnum]
   set UnoDeck [lreplace ${UnoDeck} $pcardnum $pcardnum]
   lappend UnoHand($UnoRobot) "$pcard"
  }
  if {$Debug > 1} { unolog $UnoRobot $UnoHand($UnoRobot) }
 }




 unomsg "\0030,6 Welcome to \003 [unoad]\003"
 unomsg "\0030,10 \002$UnoPlayers\002 Players In This Round \0030,6 $RoundRobin \003"

 set UnoMode 2
 set ThisPlayer [lindex $RoundRobin 0]

 # Draw Card From Deck - First Top Card

 set DiscardPile ""
 set pcardnum [rand [llength $UnoDeck]]
 set pcard [lindex $UnoDeck $pcardnum]

 # Play Doesnt Start With A Wild Card

 while {[string range $pcard 0 0] == "W"} {
  set pcardnum [rand [llength $UnoDeck]]
  set pcard [lindex $UnoDeck $pcardnum]
 }

 set PlayCard $pcard

 set UnoDeck [lreplace ${UnoDeck} $pcardnum $pcardnum]

 set Card [CardColor $pcard]

 unomsg "[nikclr $ThisPlayer]\003 Plays First - The Top Card is $Card \003"

 set Card [CardColorAll $ThisPlayer]

 showcards $ThisPlayerIDX $Card

 set UnoStartTime [unixtime]

 # Start Auto-Skip Timer

 set UnoSkipTimer [timer $AutoSkipPeriod UnoAutoSkip]

 set UnPlayedRounds 0
 return
}

#
# Cycle a new game
#
proc UnoCycle {} {
 global UnoOn UnoMode UnoCycleTime UnoCycleTimer UnoSkipTimer
 if {$UnoOn == 0} {return}
 set UnoMode 4
 catch {killtimer $UnoSkipTimer}
 set AdTime [expr $UnoCycleTime /2]
 set UnoAdTimer [utimer $AdTime UnoScoreAdvertise]
 set UnoCycleTimer [utimer $UnoCycleTime UnoNext]
 return
}

#
# Add a player
#
proc JoinUno {nick uhost hand chan arg} {
 global Debug UnoIDX UnoMode UnoPlayers RoundRobin UnoDeck UnoHand UnoChan NickColor UnoMaxPlayers

 if {($chan != $UnoChan)||($UnoMode < 1)||($UnoMode > 2)} {return}

 if {[llength $RoundRobin] == $UnoMaxPlayers} {
  unontc $nick "Sorry $nick - There Are Already $UnoMaxPlayers Players! Join In The Next Game ;)"
  return
 }

 set pcount 0
 while {[lindex $RoundRobin $pcount] != ""} {
  if {[lindex $RoundRobin $pcount] == $nick} {
   return
  }
  incr pcount
 }

 incr UnoPlayers

 lappend RoundRobin $nick
 lappend UnoIDX $nick

 if [info exist UnoHand($nick)] {unset UnoHand($nick)}
 if [info exist NickColor($nick)] {unset NickColor($nick)}

 set UnoHand($nick) ""
 set NickColor($nick) [colornick $UnoPlayers]

 # Re-Shuffle Deck

 UnoShuffle 7

 # Deal Cards To Player

 set Card ""
 while {[llength $UnoHand($nick)] != 7} {
  set pcardnum [rand [llength $UnoDeck]]
  set pcard [lindex $UnoDeck $pcardnum]
  set UnoDeck [lreplace ${UnoDeck} $pcardnum $pcardnum]
  lappend UnoHand($nick) $pcard
  append Card [CardColor $pcard]
 }

 if {$Debug > 1} { unolog $nick $UnoHand($nick) }

 unomsg "[nikclr $nick]\003 join's [unoad]\003 Table"
 unontc $nick "Hand: $Card"
 return
}

#
# Add card(s) to players hand
#
proc UnoAddDrawToHand {cplayer idx num} {
 global UnoHand UnoDeck RoundRobin CardStats

 # Check if deck needs reshuffling
 UnoShuffle $num

 set Card ""

 set newhand [expr [llength $UnoHand($cplayer)] + $num]

 while {[llength $UnoHand($cplayer)] != $newhand} {
  set pcardnum [rand [llength $UnoDeck]]
  set pcard [lindex $UnoDeck $pcardnum]
  set UnoDeck [lreplace ${UnoDeck} $pcardnum $pcardnum]
  lappend UnoHand($cplayer) $pcard
  append Card [CardColor $pcard]
 }

 showdraw $idx $Card

 incr CardStats(drawn) $num
}

#
# Remove played card from player's hand
#
proc UnoRemoveCardFromHand {cplayer pcard} {
 global UnoHand
 set UnoHand($cplayer) [lreplace $UnoHand($cplayer) $pcard $pcard]
}

#
# Add card to discard pile
#
proc AddToDiscardPile {playcard} {
 global DiscardPile
 if {[string range $playcard 1 1] != ""} {
  lappend DiscardPile $playcard
 }
}

#
# Draw a card
#
proc UnoDraw {nick uhost hand chan arg} {
 global UnoChan UnoMode UnoDeck ThisPlayer ThisPlayerIDX UnoHand RoundRobin IsDraw CardStats

 if {($chan != $UnoChan)||($UnoMode != 2)||($nick != $ThisPlayer)} {return}

 if {$IsDraw == 0} {
  set IsDraw 1
  UnoShuffle 1

  set dcardnum [rand [llength $UnoDeck]]
  set dcard [lindex $UnoDeck $dcardnum]
  lappend UnoHand($nick) $dcard
  set UnoDeck [lreplace ${UnoDeck} $dcardnum $dcardnum]

  append Card [CardColor $dcard]

  showdraw $ThisPlayerIDX $Card

  showwhodrew $nick
  incr CardStats(drawn)
  UnoAutoSkipReset

  return
 }

 unontc $nick "You have already drawn a Card $nick ! Play or Pass!"

 UnoAutoSkipReset

 return
}







#
# Pass a turn
#
proc UnoPass {nick uhost hand chan arg} {
 global UnoChan UnoMode ThisPlayer IsDraw ThisPlayerIDX RoundRobin IsColorChange CardStats

 if {($chan != $UnoChan)||($UnoMode != 2)} {return}
 if {($nick != $ThisPlayer)||($IsColorChange == 1)} {return}

 UnoAutoSkipReset

 if {$IsDraw == 1} {
  incr CardStats(passed)
  set IsDraw 0
  UnoNextPlayer
  playpass $nick $ThisPlayer
  set Card [CardColorAll $ThisPlayer]
  showcards $ThisPlayerIDX $Card
  UnoRobotRestart
 } {
  unontc $nick "Sorry $nick - First draw a card then pass"
 }
 return
}

#
# Color change
#
proc UnoColorChange {nick uhost hand chan arg} {
 global UnoChan UnoMode IsDraw PlayCard ColorPicker IsColorChange ThisPlayer ThisPlayerIDX RoundRobin

 if {($chan != $UnoChan)||($UnoMode != 2)} {return}
 if {($nick != $ColorPicker)||($IsColorChange == 0)} {return}

 UnoAutoSkipReset

 regsub -all \[`.,!{}] $arg "" arg

 set NewColor [string toupper [string range $arg 0 0]]

 switch $NewColor {
  "B" { set PlayCard "B"; set Card " \0030,12 Blue \003 "}
  "G" { set PlayCard "G"; set Card " \0030,3 Green \003 "}
  "Y" { set PlayCard "Y"; set Card " \0031,8 Yellow \003 "}
  "R" { set PlayCard "R"; set Card " \0030,4 Red \003 "}
  default { unontc $nick "Choose a Valid Colour like - Red, Green, Blue, or Yellow"; return }
 }

 UnoNextPlayer

 unomsg "[nikclr $ColorPicker]\003 chose $Card and further go with [nikclr $ThisPlayer]\003"

 set Card [CardColorAll $ThisPlayer]
 showcards $ThisPlayerIDX $Card

 set ColorPicker ""
 set IsColorChange 0
 set IsDraw 0

 UnoRobotRestart

 return
}

#
# Skip card
#
proc PlayUnoSkipCard {nick pickednum crd} {
 global IsDraw ThisPlayer ThisPlayerIDX PlayCard RoundRobin CardStats

 set c0 [string range $crd 0 0]
 set c1 [string range $crd 1 1]
 set cip0 [string range $PlayCard 0 0]
 set cip1 [string range $PlayCard 1 1]

 if {$c1 != "S"} {return 0}

 if {($c0 != $cip0)&&($c1 != $cip1)} {return 0}

 incr CardStats(played)
 incr CardStats(skips)

 AddToDiscardPile $PlayCard

 UnoRemoveCardFromHand $nick $pickednum

 set PlayCard $crd
 set Card [CardColor $crd]

 set SkipPlayer $ThisPlayer

 UnoNextPlayer

 set SkippedPlayer [lindex $RoundRobin $ThisPlayerIDX]

 UnoNextPlayer

 # No Cards Left = Winner
 if {[check_unowin $SkipPlayer $Card] > 0} {
  showwin $SkipPlayer $Card
  UnoWin $SkipPlayer
  UnoCycle
  return 1
 }

 playskip $nick $Card $SkippedPlayer $ThisPlayer

 check_hasuno $SkipPlayer

 set Card [CardColorAll $ThisPlayer]
 showcards $ThisPlayerIDX $Card

 set IsDraw 0
 return 1
}

#
# Reverse card
#
proc PlayUnoReverseCard {nick pickednum crd} {
 global IsDraw UnoIDX ThisPlayer ThisPlayerIDX PlayCard RoundRobin CardStats

 set c0 [string range $crd 0 0]
 set c1 [string range $crd 1 1]
 set cip0 [string range $PlayCard 0 0]
 set cip1 [string range $PlayCard 1 1]

 if {$c1 != "R"} {return 0}

 if {($c0 != $cip0)&&($c1 != $cip1)} {return 0}

 incr CardStats(played)
 incr CardStats(revs)

 AddToDiscardPile $PlayCard

 UnoRemoveCardFromHand $nick $pickednum

 set PlayCard $crd
 set Card [CardColor $crd]

 # Reverse RoundRobin and Move To Next Player
 set NewRoundRobin ""
 set OrigOrderLength [llength $RoundRobin]
 set IDX $OrigOrderLength

 while {$OrigOrderLength != [llength $NewRoundRobin]} {
  set IDX [expr ($IDX - 1)]
  lappend NewRoundRobin [lindex $RoundRobin $IDX]
 }

 set Newindexorder ""
 set OrigindexLength [llength $UnoIDX]
 set IDX $OrigindexLength

 while {$OrigindexLength != [llength $Newindexorder]} {
  set IDX [expr ($IDX - 1)]
  lappend Newindexorder [lindex $UnoIDX $IDX]
 }

 set UnoIDX $Newindexorder
 set RoundRobin $NewRoundRobin

 set ReversePlayer $ThisPlayer

 # Next Player After Reversing RoundRobin

 set pcount 0
 while {$pcount != [llength $RoundRobin]} {
  if {[lindex $RoundRobin $pcount] == $ThisPlayer} {
   set ThisPlayerIDX $pcount
   break
  }
  incr pcount
 }

 # <3 Players Act Like A Skip Card

 if {[llength $RoundRobin] > 2} {
  incr ThisPlayerIDX
  if {$ThisPlayerIDX >= [llength $RoundRobin]} {set ThisPlayerIDX 0}
 }

 set ThisPlayer [lindex $RoundRobin $ThisPlayerIDX]

 # No Cards Left = Winner
 if {[check_unowin $ReversePlayer $Card] > 0} {
  showwin $ReversePlayer $Card
  UnoWin $ReversePlayer
  UnoCycle
  return 1
 }

 playcard $nick $Card $ThisPlayer

 check_hasuno $ReversePlayer

 set Card [CardColorAll $ThisPlayer]
 showcards $ThisPlayerIDX $Card

 set IsDraw 0
 return 1
}

#
# Draw Two card
#
proc PlayUnoDrawTwoCard {nick pickednum crd} {
 global IsDraw ThisPlayer ThisPlayerIDX PlayCard RoundRobin CardStats UnoWildDrawTwos

 set CardOk 0
 set c0 [string range $crd 0 0]
 set c2 [string range $crd 2 2]
 set cip0 [string range $PlayCard 0 0]
 set cip1 [string range $PlayCard 1 1]
 set cip2 [string range $PlayCard 2 2]

 if {$c2 != "T"} {return 0}

 if {$c0 == $cip0} {set CardOk 1}
 if {$cip2 == "T"} {set CardOk 1}
 if {$UnoWildDrawTwos != 0} {
  if {($cip1 != "")&&($cip2 != "F")} {set CardOk 1}
 }

 if {$CardOk == 1} {
  incr CardStats(draws)
  incr CardStats(played)

  AddToDiscardPile $PlayCard

  UnoRemoveCardFromHand $nick $pickednum

  set PlayCard $crd
  set Card [CardColor $crd]

  set DrawPlayer $ThisPlayer
  set DrawPlayerIDX $ThisPlayerIDX

  # Move to the player that draws

  UnoNextPlayer

  set PlayerThatDrew $ThisPlayer
  set PlayerThatDrewIDX $ThisPlayerIDX

  # Move To The Next Player

  UnoNextPlayer

  if {[check_unowin $nick $Card] > 0} {
   UnoAddDrawToHand $PlayerThatDrew $PlayerThatDrewIDX 2
   showwin $nick $Card
   UnoWin $nick
   UnoCycle
   return 1
  }

  playdraw $nick $Card $PlayerThatDrew $ThisPlayer

  UnoAddDrawToHand $PlayerThatDrew $PlayerThatDrewIDX 2

  check_hasuno $nick

  set Card [CardColorAll $ThisPlayer]
  showcards $ThisPlayerIDX $Card

  set IsDraw 0
  return 1
 }
 return 0
}

#
# Wild Draw Four card
#
proc PlayUnoWildDrawFourCard {nick pickednum crd isrobot} {
 global ThisPlayer ThisPlayerIDX PlayCard RoundRobin IsColorChange ColorPicker CardStats

 if {[string range $crd 2 2] != "F"} {return 0}

 incr CardStats(wilds)
 incr CardStats(played)

 set ColorPicker $ThisPlayer

 AddToDiscardPile $PlayCard

 UnoRemoveCardFromHand $nick $pickednum

 set PlayCard $crd
 set Card [CardColor $crd]

 # move to the player that draws
 UnoNextPlayer

 set PlayerThatDrew $ThisPlayer
 set PlayerThatDrewIDX $ThisPlayerIDX

 if {$isrobot > 0} {
  # choose color and move to next player
  set cip [UnoBotPickAColor]
  UnoNextPlayer
 }

 if {[check_unowin $nick $Card] > 0} {
  UnoAddDrawToHand $PlayerThatDrew $PlayerThatDrewIDX 4
  showwin $nick $Card
  UnoWin $nick
  UnoCycle
  return 1
 }

 if {$isrobot > 0} {
  botplaywildfour $ColorPicker $PlayerThatDrew $ColorPicker $cip $ThisPlayer
  set ColorPicker ""
  set IsColorChange 0
 } {
  playwildfour $nick $PlayerThatDrew $ColorPicker
  set IsColorChange 1
 }

 UnoAddDrawToHand $PlayerThatDrew $PlayerThatDrewIDX 4

 check_hasuno $nick

 if {$isrobot > 0} {
  set Card [CardColorAll $ThisPlayer]
  showcards $ThisPlayerIDX $Card
 }

 set IsDraw 0
 return 1
}

#
# Wild card
#
proc PlayUnoWildCard {nick pickednum crd isrobot} {
 global IsDraw ThisPlayer ThisPlayerIDX PlayCard RoundRobin IsColorChange ColorPicker CardStats

 if {[string range $crd 0 0] != "W"} {return 0}

 incr CardStats(wilds)
 incr CardStats(played)

 set ColorPicker $ThisPlayer

 AddToDiscardPile $PlayCard

 UnoRemoveCardFromHand $nick $pickednum

 set PlayCard $crd
 set Card [CardColor $crd]

 # Ok to remove this?
 #set ThisPlayer [lindex $RoundRobin $ThisPlayerIDX]
 #set DrawnPlayer $ThisPlayer
 
 if {$isrobot > 0} {
  # Make A Color Choice
  set cip [UnoBotPickAColor]
  UnoNextPlayer
 }

 # No Cards Left = Winner
 if {[check_unowin $nick $Card] > 0} {
  showwin $nick $Card
  UnoWin $nick
  UnoCycle
  return 1
 }

 if {$isrobot > 0} {
  botplaywild $nick $ColorPicker $cip $ThisPlayer
  set ColorPicker ""
  set Card [CardColorAll $ThisPlayer]
  showcards $ThisPlayerIDX $Card
  set IsColorChange 0
 } {
  playwild $nick $ColorPicker
  set IsColorChange 1
 }

 check_hasuno $nick

 set IsDraw 0
 return 1
}

#
# Number card
#
proc PlayUnoNumberCard {nick pickednum crd} {
 global IsDraw ThisPlayer ThisPlayerIDX PlayCard RoundRobin CardStats

 set CardOk 0
 set c1 [string range $crd 0 0]
 set c2 [string range $crd 1 1]
 set cip1 [string range $PlayCard 0 0]
 set cip2 [string range $PlayCard 1 1]

 if {$c2 == -1} {return 0}

 if {$c1 == $cip1} {set CardOk 1}

 if {($cip2 != "")} {
  if {$c2 == $cip2} {set CardOk 1}
 }

 if {$CardOk == 1} {
  incr CardStats(played)
  AddToDiscardPile $PlayCard

  UnoRemoveCardFromHand $nick $pickednum

  set PlayCard $crd
  set Card [CardColor $crd]

  set NumberCardPlayer $ThisPlayer

  UnoNextPlayer

  if {[check_unowin $NumberCardPlayer $Card] > 0} {
   showwin $NumberCardPlayer $Card
   UnoWin $NumberCardPlayer
   UnoCycle
   return 1
  }

  playcard $nick $Card $ThisPlayer

  check_hasuno $NumberCardPlayer

  set Card [CardColorAll $ThisPlayer]
  showcards $ThisPlayerIDX $Card

  set IsDraw 0
  return 1
 }
 unontc $nick "No valid card - Draw or play another!"
 return 0
}

#
# Attempt to find card in hand
#
proc UnoFindCard {nick pickednum crd IsRobot} {
 global UnoRobot ThisPlayer ThisPlayerIDX

  #if {$Debug > 1} {unolog $UnoRobot "UnoFindCard: [lindex $UnoHand($ThisPlayer) $pickednum"}

  # Wild Draw Four
  set FoundCard [PlayUnoWildDrawFourCard $nick $pickednum $crd $IsRobot]
  if {$FoundCard == 1} {return 4}

  # Wild
  set FoundCard [PlayUnoWildCard $nick $pickednum $crd $IsRobot]
  if {$FoundCard == 1} {return 5}

  # Draw Two
  set FoundCard [PlayUnoDrawTwoCard $nick $pickednum $crd]
  if {$FoundCard == 1} {return 3}

  # Skip
  set FoundCard [PlayUnoSkipCard $nick $pickednum $crd]
  if {$FoundCard == 1} {return 1}

  # Reverse
  set FoundCard [PlayUnoReverseCard $nick $pickednum $crd]
  if {$FoundCard == 1} {return 2}

  # Number card
  set FoundCard [PlayUnoNumberCard $nick $pickednum $crd]
  if {$FoundCard == 1} {return 6}

  return 0
}








#
# Play a card
#
proc UnoPlayCard {nick uhost hand chan arg} {
 global UnoChan UnoMode IsDraw IsColorChange ColorPicker UnoPlayers RoundRobin UnoHand ThisPlayer

 if {($chan != $UnoChan)||($UnoMode != 2)||($nick != $ThisPlayer)} {return}

 UnoAutoSkipReset

 if {$IsColorChange == 1} {return}

 regsub -all \[`,.!{}\ ] $arg "" arg

 if {$arg == ""} {return}

 set pcard [string toupper [string range $arg 0 3]]

 set CardInPlayerHand 0

 set pcount 0
 while {[lindex $UnoHand($nick) $pcount] != ""} {
  if {$pcard == [lindex $UnoHand($nick) $pcount]} {
   set pcardnum $pcount
   set CardInPlayerHand 1
   break
  }
  incr pcount
 }

 if {$CardInPlayerHand == 0} {
  unontc $nick "Sorry $nick - You cant play this card - Draw or play another"
  return
 }

 set CardFound [UnoFindCard $nick $pcardnum $pcard 0]

 switch $CardFound {
  0 {return}
  4 {return}
  5 {return}
  default {UnoRobotRestart; return}
 }
}

#
# Robot Player
#

proc UnoRobotPlayer {} {
 global Debug UnoIDX IsDraw IsColorChange ColorPicker UnoMode UnoPlayers RoundRobin UnoDeck UnoHand ThisPlayer ThisPlayerIDX PlayCard CardStats UnoRobot

 # Check for a valid card in hand
 set CardOk 0
 set IsDraw 0
 set CardCount 0

 set cip1 [string range $PlayCard 0 0]
 set cip2 [string range $PlayCard 1 1]

 while {$CardCount < [llength $UnoHand($ThisPlayer)]} {
  set playcard [lindex $UnoHand($ThisPlayer) $CardCount]
  set c1 [string range $playcard 0 0]
  set c2 [string range $playcard 1 1]

  #if {$Debug > 1} {unolog $UnoRobot "Trying: $playcard"}

  if {($c1 == $cip1)||($c2 == $cip2)||($c1 == "W")} {
   set CardOk 1
   set pcard $playcard
   set pcardnum $CardCount
   break
  }
  incr CardCount
 }

 # Play the card if found
 if {$CardOk == 1} {
  set CardFound [UnoFindCard $UnoRobot $pcardnum $pcard 1]
  switch $CardFound {
   0 {}
   5 {return}
   6 {return}
   default {UnoRobotRestart; return}
  }
 }

 # Bot draws a card

 UnoShuffle 1

 set dcardnum [rand [llength $UnoDeck]]
 set dcard [lindex $UnoDeck $dcardnum]
 lappend UnoHand($UnoRobot) "$dcard"
 set UnoDeck [lreplace ${UnoDeck} $dcardnum $dcardnum]

 showwhodrew $UnoRobot

 set CardOk 0
 set CardCount 0

 incr CardStats(drawn)

 while {$CardCount < [llength $UnoHand($ThisPlayer)]} {
  set playcard [lindex $UnoHand($ThisPlayer) $CardCount]
  set c1 [string range $playcard 0 0]
  set c2 [string range $playcard 1 1]

  # if {$Debug > 1} {unolog $UnoRobot "DrawTry: $playcard"}

  if {($c1 == $cip1)||($c2 == $cip2)||($c1 == "W")} {
   set CardOk 1
   set pcard $playcard
   set pcardnum $CardCount
   break
  }
  incr CardCount
 }

 # Bot plays drawn card or passes turn

 if {$CardOk == 1} {
  set CardFound [UnoFindCard $UnoRobot $pcardnum $pcard 1]
  if {$CardFound == 1} {UnoRobotRestart; return}
  switch $CardFound {
   0 {}
   5 {return}
   6 {return}
   default {UnoRobotRestart; return}
  }
 } {
  incr CardStats(passed)

  set IsDraw 0

  UnoNextPlayer

  playpass $UnoRobot $ThisPlayer

  set Card [CardColorAll $ThisPlayer]
  showcards $ThisPlayerIDX $Card
 }
 return
}

#
# Autoskip inactive players
#
proc UnoAutoSkip {} {
 global UnoMode ThisPlayer ThisPlayerIDX RoundRobin AutoSkipPeriod IsColorChange ColorPicker
 global UnoIDX UnoPlayers UnoDeck UnoHand UnoChan UnoSkipTimer Debug NickColor UnoPaused

 if {$UnoMode != 2} {return}
 if {$UnoPaused != 0} {return}

 if {[uno_isrobot $ThisPlayerIDX]} {return}

 set Idler $ThisPlayer
 set IdlerIDX $ThisPlayerIDX

 if {[unotimerexists UnoAutoSkip] != ""} {
  unolog "uno" "AutoSkip Timer Already Exists"
  return
 }

 set InChannel 0
 set uclist [chanlist $UnoChan]

 set pcount 0
 while {[lindex $uclist $pcount] != ""} {
  if {[lindex $uclist $pcount] == $Idler} {
   set InChannel 1
   break
  }
  incr pcount
 }

 if {$InChannel == 0} {
  unomsg "[nikclr $Idler]\003 has left the channel and we now of ALL other players GEWUERGT! \003"

  if {$IsColorChange == 1} {
   if {$Idler == $ColorPicker} {
    # Make A Color Choice
    set cip [UnoPickAColor]
    unomsg "\0030,13 $Idler \003was picking a color : randomly selecting $cip \003"
    set IsColorChange 0
   } {
    unolog "uno" "UnoAutoRemove: IsColorChange set but $Idler not ColorPicker"
   }
  }

  UnoNextPlayer
  unomsg "[nikclr $Idler]\003 war dran, weiter mit [nikclr $ThisPlayer]\003"
  if {![uno_isrobot $ThisPlayerIDX]} {
   set Card [CardColorAll $ThisPlayer]
   showcards $ThisPlayerIDX $Card
  }

  set UnoPlayers [expr ($UnoPlayers -1)]

  # Remove Player From Game And Put Cards Back In Deck
  if {$UnoPlayers > 1} {
   set RoundRobin [lreplace ${RoundRobin} $IdlerIDX $IdlerIDX]
   set UnoIDX [lreplace ${UnoIDX} $IdlerIDX $IdlerIDX]
   lappend UnoDeck "$UnoHand($Idler)"
   unset UnoHand($Idler)
   unset NickColor($Idler)
  }

  switch $UnoPlayers {
   1 {
      showwindefault $ThisPlayer
      UnoWin $ThisPlayer
      UnoCycle
     }
   0 {
      unomsg "[unoad] \0030,10 No players - No winners - Rehashing Now? \003"
      UnoCycle
     }
   default {
      if {![uno_isrobot $ThisPlayerIDX]} {
       UnoAutoSkipReset
       UnoRobotRestart
      }
     }
  }
  return
 }

 if {$Debug > 0} {unolog "uno" "AutoSkip Player: $Idler"}

 unomsg "[nikclr $Idler]\003 idle for \00313$AutoSkipPeriod \003 Minutes and is skipped"




 # Player Was ColorPicker

 if {$IsColorChange == 1} {
  if {$Idler == $ColorPicker} {
   # Make A Color Choice
   set cip [UnoPickAColor]
   unomsg "[nikclr $Idler]\003 choose a color : randomly selected $cip"
   set IsColorChange 0
  } {
   unolog "uno" "UnoRemove: IsColorChange set but $Idler not ColorPicker"
  }
 }

 UnoNextPlayer

 unomsg "[nikclr $Idler]\003 grad was tuned - with more [nikclr $ThisPlayer]\003"

 if {![uno_isrobot $ThisPlayerIDX]} {
  set Card [CardColorAll $ThisPlayer]
  showcards $ThisPlayerIDX $Card
 } {
  UnoRobotRestart
 }
 UnoAutoSkipReset
 return
}

#
# Pause play
#
proc UnoPause {nick uhost hand chan arg} {
 global UnoChan UnoOn UnoMode UnoOpFlags UnoPaused

 if {$chan != $UnoChan} {return}
 if {$UnoOn != 1} {return}
 if {$UnoMode != 2} {return}

 if {([validuser $nick])&&([matchattr $nick $UnoOpFlags $UnoChan])} {
  if {$UnoPaused == 0} {
   set UnoPaused 1
   UnoUnbindCmds
   unomsg "[unoad] \0030,4 removed by $nick \003"
  } {
   set UnoPaused 0
   UnoBindCmds
   UnoAutoSkipReset
   unomsg "[unoad] \0030,4 added by by $nick \003"
  }
 }
}

#
# Remove user from play
#
proc UnoRemove {nick uhost hand chan arg} {
 global UnoChan UnoOn UnoCycleTime UnoIDX UnoPlayers ThisPlayer ThisPlayerIDX RoundRobin UnoDeck DiscardPile UnoHand IsColorChange ColorPicker NickColor UnoOpFlags

 if {$chan != $UnoChan} {return}
 if {$UnoOn == 0} {return}

 regsub -all \[`,.!{}] $arg "" arg

 # Allow Ops To Remove Another Player
 set UnoOpRemove 0

 if {[string length $arg] > 0} {
  if {([validuser $nick])&&([matchattr $nick $UnoOpFlags $UnoChan])} {
   set UnoOpRemove 1
   set UnoOpNick $nick
   set nick $arg
  } {
   return
  }
 }

 set PlayerFound 0

 # Remove Player If Found - Put Cards Back To Bottom Of Deck

 set pcount 0
 while {[lindex $RoundRobin $pcount] != ""} {
  if {[string tolower [lindex $RoundRobin $pcount]] == [string tolower $nick]} {
   set PlayerFound 1
   set FoundIDX $pcount
   set nick [lindex $RoundRobin $pcount]
   break
  }
  incr pcount
 }

 if {$PlayerFound == 1} {
  if {$UnoOpRemove > 0} {
   unomsg "[nikclr $nick]\003 Mad Person was removed from Uno $UnoOpNick"
  } {
   unontc $nick "You have been disqualified From UNO!"
   unomsg "[nikclr $nick]\003 Stupid has left Uno"
  }

  # Player Was ColorPicker

  if {$IsColorChange == 1} {
   if {$nick == $ColorPicker} {
    # Make A Color Choice
    set cip [UnoPickAColor]
    unomsg "[nikclr $nick]\003 choose a color... randomly select $cip"
    set IsColorChange 0
   } {
    unolog "uno" "UnoRemove: IsColorChange Set but $nick not ColorPicker"
   }
  }

  if {$nick == $ThisPlayer} {
   UnoNextPlayer
   if {$UnoPlayers > 2} {
    unomsg "[nikclr $nick]\003 grad was tuned - continue with [nikclr $ThisPlayer]\003"
   }
   UnoAutoSkipReset
  }

  set UnoPlayers [expr ($UnoPlayers -1)]

  # Remove Player From Game And Put Cards Back In Deck

  if {$UnoPlayers > 1} {
   set RoundRobin [lreplace ${RoundRobin} $FoundIDX $FoundIDX]
   set UnoIDX [lreplace ${UnoIDX} $FoundIDX $FoundIDX]
   lappend DiscardPile "$UnoHand($nick)"
   unset UnoHand($nick)
   unset NickColor($nick)
  }

  set pcount 0
  while {[lindex $RoundRobin $pcount] != ""} {
   if {[lindex $RoundRobin $pcount] == $ThisPlayer} {
    set ThisPlayerIDX $pcount
    break
   }
   incr pcount
  }

  if {$UnoPlayers == 1} {
   showwindefault $ThisPlayer
   UnoWin $ThisPlayer
   UnoCycle
   return
  }
  UnoRobotRestart
 } {
  # Player not in current game
  return
 }

 if {$UnoPlayers == 0} {
  unomsg "[unoad] \0030,10 No Players - No Winners - What now? \003"
  UnoCycle
 }
 return
}









#
# Move to next player
#
proc UnoNextPlayer {} {
 global ThisPlayer ThisPlayerIDX RoundRobin
 incr ThisPlayerIDX
 if {$ThisPlayerIDX >= [llength $RoundRobin]} {set ThisPlayerIDX 0}
 set ThisPlayer [lindex $RoundRobin $ThisPlayerIDX]
}

#
# Pick a random color for skipped/removed players
#
proc UnoPickAColor {} {
 global PlayCard
 set ucolors "r g b y"
 set pcol [string tolower [lindex $ucolors [rand [llength $ucolors]]]]
 switch $pcol {
  "r" {set PlayCard "R"; return "\0030,4 Red \003"}
  "g" {set PlayCard "G"; return "\0030,3 Green \003"}
  "b" {set PlayCard "B"; return "\0030,12 Blue \003"}
  "y" {set PlayCard "Y"; return "\0031,8 Yellow \003"}
 }
}

#
# Robot picks a color by checking hand for 1st color card
# found with matching color, else picks color at random
#
proc UnoBotPickAColor {} {
 global PlayCard UnoHand ThisPlayer
 set CardCount 0
 while {$CardCount < [llength $UnoHand($ThisPlayer)]} {
  set thiscolor [string range [lindex $UnoHand($ThisPlayer) $CardCount] 0 0]
  switch $thiscolor {
   "R" {set PlayCard "R"; return "\0030,4 Red \003"}
   "G" {set PlayCard "G"; return "\0030,3 Green \003"}
   "B" {set PlayCard "B"; return "\0030,12 Blue \003"}
   "Y" {set PlayCard "Y"; return "\0031,8 Yellow \003"}
  }
  incr CardCount
 }
 set ucolors "r g b y"
 set pcol [string tolower [lindex $ucolors [rand [llength $ucolors]]]]
 switch $pcol {
  "r" {set PlayCard "R"; return "\0030,4 Red \003"}
  "g" {set PlayCard "G"; return "\0030,3 Green \003"}
  "b" {set PlayCard "B"; return "\0030,12 Blue \003"}
  "y" {set PlayCard "Y"; return "\0031,8 Yellow \003"}
 }
}

#
# Set robot for next turn
#
proc UnoRobotRestart {} {
 global UnoMode ThisPlayerIDX RobotRestartPeriod UnoBotTimer
 if {$UnoMode != 2} {return}
 if {![uno_isrobot $ThisPlayerIDX]} {return}
 set UnoBotTimer [utimer $RobotRestartPeriod UnoRobotPlayer]
}

#
# Reset autoskip timer
#
proc UnoAutoSkipReset {} {
 global AutoSkipPeriod UnoMode UnoSkipTimer
 catch {killtimer $UnoSkipTimer}
 if {$UnoMode == 2} {
  set UnoSkipTimer [timer $AutoSkipPeriod UnoAutoSkip]
 }
}

#
# Channel triggers
#

#
# Show current player order
#
proc UnoOrder {nick uhost hand chan arg} {
 global UnoChan UnoMode UnoPlayers RoundRobin
 if {($chan != $UnoChan)||($UnoMode < 2)} {return}
 unomsg "\0030,10 Order of play \[$UnoPlayers\]: \0030,6 $RoundRobin "
 return
}

#
# Show game running time
#
proc UnoTime {nick uhost hand chan arg} {
global UnoChan UnoMode
 if {($chan != $UnoChan)||($UnoMode != 2)} {return}
 set unotime "\0030,10 playing time: \0030,6 [duration [game_time]] \003"
 unomsg "$unotime"
 return
}

#
# Show player what cards they hold
#
proc UnoShowCards {nick uhost hand chan arg} {
 global UnoChan UnoMode UnoHand ThisPlayerIDX
 if {($chan != $UnoChan)||($UnoMode != 2)} {return}
 if [info exist UnoHand($nick)] {
  set Card ""
  set ccnt 0
  while {[llength $UnoHand($nick)] != $ccnt} {
   set pcard [lindex $UnoHand($nick) $ccnt]
   append Card [CardColor $pcard]
   incr ccnt
  }
  if {![uno_isrobot $ThisPlayerIDX]} {
   unontc $nick "Hand: $Card\003"
  }
 }
 return
}

#
# Show current player
#
proc UnoTurn {nick uhost hand chan arg} {
 global UnoChan UnoMode ThisPlayer RoundRobin UnoMode
 if {($chan != $UnoChan)||($UnoMode != 2)} {return}
 if {[llength $RoundRobin] < 1 } {return}
 set info "\0030,10 current player: \0030,6 $ThisPlayer \003"
 unomsg "$info"
 return
}

#
# Show current top card
#
proc UnoTopCard {nick uhost hand chan arg} {
 global PlayCard UnoChan UnoMode
 if {($chan != $UnoChan)||($UnoMode != 2)} {return}
 set pcard $PlayCard
 set Card [CardColor $pcard]
 unomsg "\0030,10 card in play: \003 $Card \003"
 return
}

#
# Show card stats
#
proc UnoCardStats {nick uhost hand chan arg} {
 global UnoChan UnoMode CardStats
 if {($chan != $UnoChan)||($UnoMode != 2)} {return}
 set passdraw [format "%3.1f" [get_ratio $CardStats(passed) $CardStats(drawn)]]
 set skiprev [expr $CardStats(skips) +$CardStats(revs)]
 unomsg "\0030,10 Card Stats: \0030,6 played:$CardStats(played)  Pass\/Draw Ratio:$passdraw\%  Skip\/Rev:$skiprev  DrawCards:$CardStats(draws)  WildCards:$CardStats(wilds) \003"
 return
}

#
# Card count
#
proc UnoCount {nick uhost hand chan arg} {
 global RoundRobin UnoHand UnoMode UnoChan
 if {($chan != $UnoChan)||($UnoMode != 2)} {return}
 set ordcnt 0
 set crdcnt ""
 while {[lindex $RoundRobin $ordcnt] != ""} {
  append crdcnt "\0030,10 [lindex $RoundRobin $ordcnt] \0030,6 [llength $UnoHand([lindex $RoundRobin $ordcnt])] Cards "
  incr ordcnt
 }
 unomsg "$crdcnt\003"
 return
}

#
# Show player's score
#
proc UnoWon {nick uhost hand chan arg} {
 global UnoScoreFile UnoPointsName

 regsub -all \[`,.!] $arg "" arg

 if {[string length $arg] == 0} {set arg $nick}

 set scorer [string tolower $arg]

 set pflag 0

 set f [open $UnoScoreFile r]
 while {[gets $f sc] != -1} {
  set cnick [string tolower [lindex [split $sc] 0]]
  if {$cnick == $scorer} {
   set pmsg "\0030,10 [lindex [split $sc] 0] \0030,6 [lindex $sc 2] $UnoPointsName in [lindex $sc 1] Games \003"
   set pflag 1
  }
 }
 close $f

 if {$pflag == 0} {
  set pmsg "\0030,10 $arg \0030,6 No points \003"
 }
 unomsg "$pmsg"
 return
}

#
# Display current top10
#
proc UnoTopTen {nick uhost hand chan arg} {
 global UnoChan 
 if {$chan != $UnoChan} {return}
 UnoTop10 1
 return
}
proc UnoTopTenWon {nick uhost hand chan arg} {
 global UnoChan 
 if {$chan != $UnoChan} {return}
 UnoTop10 0
 return
}

#
# Display last month's top3
proc UnoTopThreeLast {nick uhost hand chan arg} {
 global UnoChan 
 if {$chan != $UnoChan} {return}
 UnoLastMonthTop3 $nick $uhost $hand $chan 0
 unomsg "0,4 UnoPoint - People's Choice "
 UnoLastMonthTop3 $nick $uhost $hand $chan 1
 return
}

#
# Display month fastest game 
#
proc UnoTopFast {nick uhost hand chan arg} {
 global UnoChan UnoFast
 if {$chan != $UnoChan} {return}
 unomsg "\0030,6 Fastest Game of The Month \0030,10 [lindex [split $UnoFast] 0] [duration [lindex $UnoFast 1]] \003"
 return
}

#
# Display month high score
#
proc UnoHighScore {nick uhost hand chan arg} {
 global UnoChan UnoHigh UnoPointsName
 if {$chan != $UnoChan} {return}
 unomsg "\0030,6 High-Score of The Month \0030,10 [lindex [split $UnoHigh] 0] [lindex $UnoHigh 1] $UnoPointsName \003"
 return
}

#
# Display month most cards played
#
proc UnoPlayed {nick uhost hand chan arg} {
 global UnoChan UnoPlayed
 if {$chan != $UnoChan} {return}
 unomsg "\0030,6 Most cards Played of The Month \0030,10 [lindex [split $UnoPlayed] 0] [lindex $UnoPlayed 1] Cards \003"
 return
}

#
# Show all-time records
#
proc UnoRecords {nick uhost hand chan arg} {
 global UnoChan UnoRecordFast UnoRecordHigh UnoRecordCard UnoRecordWins UnoRecordPlayed
 if {$chan != $UnoChan} {return}
 unomsg "\0030,6 All-Time Records: points \0030,10 $UnoRecordCard \0030,6 Games \0030,10 $UnoRecordWins \0030,6 Speed \0030,10 [lindex $UnoRecordFast 0] [duration [lindex $UnoRecordFast 1]] \0030,6 High Score \0030,10 $UnoRecordHigh \0030,6 played cards \0030,10 $UnoRecordPlayed \003"
 return
}

#
# Display month top10
#
proc UnoTop10 {mode} {
 global UnoScoreFile unsortedscores UnoPointsName UnoRobot

 if {($mode < 0)||($mode > 1)} {set mode 0}

 switch $mode {
  0 {set winners "\0030,6 Top10 Points "}
  1 {set winners "\0030,6 Top10 $UnoPointsName "}
 }

 if ![file exists $UnoScoreFile] {
  set f [open $UnoScoreFile w]
  puts $f "$UnoRobot 0 0"
  unomsg "\0030,10 Points Table is empty - I'll do a new one\003"
  close $f
  return
 }

 if [info exists unsortedscores] {unset unsortedscores}
 if [info exists top10] {unset top10}

 set f [open $UnoScoreFile r]
 while {[gets $f s] != -1} {
  switch $mode {
   0 {set unsortedscores([lindex [split $s] 0]) [lindex $s 1]}
   1 {set unsortedscores([lindex [split $s] 0]) [lindex $s 2]}
  }
 }
 close $f

 for {set s 0} {$s < 10} {incr s} {
  set top10($s) "Nobody 0"
 }

 set s 0
 foreach n [lsort -decreasing -command UnoSortScores [array names unsortedscores]] {
  set top10($s) "$n $unsortedscores($n)"
  incr s
 }

 for {set s 0} {$s < 10} {incr s} {
  if {[lindex $top10($s) 1] > 0} {
   append winners "\0030,6 #[expr $s +1] \0030,10 [lindex [split $top10($s)] 0] [lindex $top10($s) 1] "
  } {
   append winners "\0030,6 #[expr $s +1] \0030,10 Nobody 0 "
  }
 }
 unomsg "$winners\003"
 return
}

#
# Last month's top3
proc UnoLastMonthTop3 {nick uhost hand chan arg} {
 global UnoChan UnoLastMonthCards UnoLastMonthGames UnoPointsName
 if {$chan != $UnoChan} {return}
 if {$arg == 0} {
  if [info exists UnoLastMonthCards] {
   set UnoTop3 "\0030,6 Last Month's Top 3 $UnoPointsName Won "
   for { set s 0} { $s < 3 } { incr s} {
    append UnoTop3 "\0030,6 #[expr $s +1] \0030,10 $UnoLastMonthCards($s) "
   }
   unomsg "$UnoTop3"
  }
 } {
  if [info exists UnoLastMonthGames] {
   set UnoTop3 "\0030,6 Last Month's Top 3 Games Won "
   for { set s 0} { $s < 3 } { incr s} {
    append UnoTop3 "\0030,6 #[expr $s +1] \0030,10 $UnoLastMonthGames($s) "
   }
   unomsg "$UnoTop3"
  }
 }
}


# BIS HIERHER GEFUNZT

#
# Show game help
#
proc UnoCmds {nick uhost hand chan arg} {
 global UnoChan
 if {$chan != $UnoChan} {return}
 putmsg $nick "Uno Commands: !uno - to start the game"
 putmsg $nick "Uno Commands: !stop - to stop the game"
 putmsg $nick "Uno Commands: !remove \[nick\] - removes a player from the player list"
 putmsg $nick "Uno Commands: !unowon \[nick\] - how many reward points you have already made"
 putmsg $nick "Uno Commands: !unocmds - list the uno-commands (the convincing here ;)"
 putmsg $nick "Uno Stats: !unotop10 !unotop3last !unofast !unohigh !unorecords"
 putmsg $nick "Uno Card Commands: jo/join - for join the game "
 putmsg $nick "Uno Card Commands: pl/play - for play a card"
 putmsg $nick "Uno Card Commands: dr/draw - for draw a card"
 putmsg $nick "Uno Card Commands: pa/pass - for passing turn"
 putmsg $nick "Uno Card Commands: co/color - Choose the color"
 putmsg $nick "Uno Chan Commands: ca/cards - what cards you have just"
 putmsg $nick "Uno Chan Commands: cd/card - which card is on top"
 putmsg $nick "Uno Chan Commands: tu/turn - who is currently playing"
 putmsg $nick "Uno Chan Commands: od/order - order of the players"
 putmsg $nick "Uno Chan Commands: ct/count - Counting of cards each player have"
 putmsg $nick "Uno Chan Commands: st/stats - how many cards players have"
 putmsg $nick "Uno Chan Commands: ti/time - how long is already played"
 putmsg $nick "And now play a lot of fun!!"
 putmsg $nick "If you still have questions should contact #Unopoint @ Dalnet!!"
 return
}

#
# Uno version
#
proc UnoVersion {nick uhost hand chan arg} {
 global UnoVersion
 unomsg "[unoad]\003 \0030,6 $UnoVersion Powered By UnoPoint Team  \003"
 return
}

#
# Read score file
#
proc UnoReadScores {} {
 global unogameswon unoptswon UnoScoreFile UnoRobot

 if [info exists unogameswon] { unset unogameswon }
 if [info exists unoptswon] { unset unoptswon }

 if ![file exists $UnoScoreFile] {
  set f [open $UnoScoreFile w]
  puts $f "$UnoRobot 0 0"
  close $f
 }

 set f [open $UnoScoreFile r]
 while {[gets $f s] != -1} {
  set unogameswon([lindex [split $s] 0]) [lindex $s 1]
  set unoptswon([lindex [split $s] 0]) [lindex $s 2]
 }
 close $f

 return
}

#
# Clear top10 and write monthly scores
#
proc UnoNewMonth {min hour day month year} {
 global unsortedscores unogameswon unoptswon UnoLastMonthCards UnoLastMonthGames UnoScoreFile UnoRobot
 global UnoFast UnoHigh UnoPlayed UnoRecordFast UnoRecordHigh UnoRecordPlayed UnoRecordCard UnoRecordWins

 set lmonth [UnoLastMonthName $month]

 unomsg "[unoad] \0030,4 Deleting monthly points \003"

 set UnoMonthFileName "$UnoScoreFile.$lmonth"

 # Read Current Scores

 UnoReadScores

 # Write To Old Month File

 if ![file exists $UnoMonthFileName] {
  set f [open $UnoMonthFileName w]
  foreach n [array names unogameswon] {
   puts $f "$n $unogameswon($n) $unoptswon($n)"
  }
  close $f
 }

 # Find Top 3 Card Holders and Game Winners

 set mode 0 

 while {$mode < 2} {
  if [info exists unsortedscores] {unset unsortedscores}
  if [info exists top10] {unset top10}

  set f [open $UnoScoreFile r]
  while {[gets $f s] != -1} {
   switch $mode {
    0 {set unsortedscores([lindex [split $s] 0]) [lindex $s 1]}
    1 {set unsortedscores([lindex [split $s] 0]) [lindex $s 2]}
   }
  }
  close $f

  set s 0
  foreach n [lsort -decreasing -command UnoSortScores [array names unsortedscores]] {
   set top10($s) "$n $unsortedscores($n)"
   incr s
  }

  for {set s 0} {$s < 3} {incr s} {
   if {[lindex $top10($s) 1] > 0} {
    switch $mode {
     0 {set UnoLastMonthGames($s) "[lindex [split $top10($s)] 0] [lindex $top10($s) 1]"}
     1 {set UnoLastMonthCards($s) "[lindex [split $top10($s)] 0] [lindex $top10($s) 1]"}
    }
   } {
    switch $mode {
     0 {set UnoLastMonthGames($s) "Nobody 0"}
     1 {set UnoLastMonthCards($s) "Nobody 0"}
    }
   }
  }
  incr mode
 }

 # Update records
 if {[lindex $UnoFast 1] < [lindex $UnoRecordFast 1]} {set UnoRecordFast $UnoFast}
 if {[lindex $UnoHigh 1] > [lindex $UnoRecordHigh 1]} {set UnoRecordHigh $UnoHigh}
 if {[lindex $UnoPlayed 1] > [lindex $UnoRecordPlayed 1]} {set UnoRecordPlayed $UnoPlayed}
 if {[lindex $UnoLastMonthCards(0) 1] > [lindex $UnoRecordCard 1]} {set UnoRecordCard $UnoLastMonthCards(0)}
 if {[lindex $UnoLastMonthGames(0) 1] > [lindex $UnoRecordWins 1]} {set UnoRecordWins $UnoLastMonthGames(0)}

 # Wipe last months records
 set UnoFast "$UnoRobot 60"
 set UnoHigh "$UnoRobot 100"
 set UnoPlayed "$UnoRobot 100"

 # Save Top3 And Records To Config File
 Uno_WriteCFG

 # Wipe This Months Score File

 set f [open $UnoScoreFile w]
 puts $f "$UnoRobot 0 0"
 close $f

 unolog "uno" "Cleared monthly scores"
 return
}

#
# Update score of winning player
#
proc UnoUpdateScore {winner cardtotals} {
 global unogameswon unoptswon UnoScoreFile

 UnoReadScores

 if {[info exists unogameswon($winner)]} {
  incr unogameswon($winner)
 } {
  set unogameswon($winner) 1
 }

 if {[info exists unoptswon($winner)]} {
  incr unoptswon($winner) $cardtotals
 } {
  set unoptswon($winner) $cardtotals
 }

 set f [open $UnoScoreFile w]
 foreach n [array names unogameswon] {
  puts $f "$n $unogameswon($n) $unoptswon($n)"
 }
 close $f

 return
}

#
# Display winner and game statistics
#
proc UnoWin {winner} {
 global UnoHand ThisPlayer RoundRobin UnoPointsName CardStats UnoMode UnoCycleTime UnoFast UnoHigh UnoPlayed UnoBonus
 
 set cardtotals 0
 set UnoMode 3
 set ThisPlayerIDX 0
 set needCFGWrite 0

 set UnoTime [game_time]

 unomsg "\0030,6 Cards Total \003"

 # Total up all player's cards

 while {$ThisPlayerIDX != [llength $RoundRobin]} {
  set Card ""
  set ThisPlayer [lindex $RoundRobin $ThisPlayerIDX]

  if {$ThisPlayer != $winner} {
   set ccount 0
   while {[lindex $UnoHand($ThisPlayer) $ccount] != ""} {
    set cardtotal [lindex $UnoHand($ThisPlayer) $ccount]
    set c1 [string range $cardtotal 0 0]
    set c2 [string range $cardtotal 1 1]
    set cardtotal 0

    if {$c1 == "W"} {
     set cardtotal 50
    } {
     switch $c2 {
      "S" {set cardtotal 20}
      "R" {set cardtotal 20}
      "D" {set cardtotal 20}
      default {set cardtotal $c2}
     }
    }
    set cardtotals [expr $cardtotals + $cardtotal]
    incr ccount
   }
   set Card [CardColorAll $ThisPlayer]
   unomsg "[strpad [nikclr $ThisPlayer] 12] $Card"
  }
  incr ThisPlayerIDX
 }

 # Check high score record
 set HighScore [lindex $UnoHigh 1]
 if {$cardtotals > $HighScore} {
  unomsg "\0030,4 $winner has broken the high-score record and get $UnoBonus Bonus $UnoPointsName \003"
  set UnoHigh "$winner $cardtotals"
  incr cardtotals $UnoBonus
  set needCFGWrite 1
 }
 # Check played cards record
 set HighPlayed [lindex $UnoPlayed 1]
 if {$CardStats(played) > $HighPlayed} {
  unomsg "\0030,4 $winner has broken the most played cards record and get $UnoBonus Bonus $UnoPointsName \003"
  set UnoPlayed "$winner $CardStats(played)"
  incr cardtotals $UnoBonus
  set needCFGWrite 1
 }
 # Check fast game record
 set FastRecord [lindex $UnoFast 1]
 if {$UnoTime < $FastRecord} {
  unomsg "\0030,4 $winner has broken the fastest game Record and get $UnoPointsName \003"
  incr cardtotals $UnoBonus
  set UnoFast "$winner $UnoTime"
  set needCFGWrite 1
 }

 # Winner
 unomsg "\0030,10 $winner \0030,6 $cardtotals $UnoPointsName In [duration $UnoTime] \003"

 # Card stats
 set passdraw [format "%3.1f" [get_ratio $CardStats(passed) $CardStats(drawn)]]
 set skiprev [expr $CardStats(skips) +$CardStats(revs)]
 unomsg "\0030,10 Cardstats \0030,6 CardsPlayed:$CardStats(played)  Pass\/Draw Ratio:$passdraw\%  Skip\/Rev:$skiprev  DrawCards:$CardStats(draws)  WildCards:$CardStats(wilds) \003"
 unomsg "[unoad] \0030,3 Next Game in $UnoCycleTime Seconds \003"

 # Write scores
 UnoUpdateScore $winner $cardtotals

 # Write records
 if {$needCFGWrite > 0} {Uno_WriteCFG}

 return
}

#
# Re-Shuffle deck
#
proc UnoShuffle {len} {
 global UnoDeck DiscardPile
 if {[llength $UnoDeck] >= $len} { return }
 unomsg "[unoad] \0030,4 You play as much - I have to reshuffle the deck! \003"
 lappend DiscardPile "$UnoDeck"
 set UnoDeck ""
 set NewDeckSize [llength $DiscardPile]
 while {[llength $UnoDeck] != $NewDeckSize} {
  set pcardnum [rand [llength $DiscardPile]]
  set pcard [lindex $DiscardPile $pcardnum]
  lappend UnoDeck "$pcard"
  set DiscardPile [lreplace ${DiscardPile} $pcardnum $pcardnum]
 }
 return
}

#
# Read config file
#
proc Uno_ReadCFG {} {
 global UnoCFGFile UnoLastMonthCards UnoLastMonthGames UnoPointsName UnoScoreFile UnoRobot UnoChan UnoFast UnoHigh UnoPlayed UnoStopAfter UnoBonus
 global UnoRecordHigh UnoRecordFast UnoRecordCard UnoRecordWins UnoRecordPlayed UnoWildDrawTwos
 if {[file exist $UnoCFGFile]} {
  set f [open $UnoCFGFile r]
  while {[gets $f s] != -1} {
   set kkey [string tolower [lindex [split $s "="] 0]]
   set kval [lindex [split $s "="] 1]
   switch $kkey {
    botname {set UnoRobot $kval}
    channel {set UnoChan $kval}
    points {set UnoPointsName $kval}
    scorefile {set UnoScoreFile $kval}
    stopafter {set UnoStopAfter $kval}
    wilddrawtwos {set UnoWildDrawTwos $kval}
    lastmonthcard1 {set UnoLastMonthCards(0) $kval}
    lastmonthcard2 {set UnoLastMonthCards(1) $kval}
    lastmonthcard3 {set UnoLastMonthCards(2) $kval}
    lastmonthwins1 {set UnoLastMonthGames(0) $kval}
    lastmonthwins2 {set UnoLastMonthGames(1) $kval}
    lastmonthwins3 {set UnoLastMonthGames(2) $kval}
    fast {set UnoFast $kval}
    high {set UnoHigh $kval}
    played {set UnoPlayed $kval}
    bonus {set UnoBonus $kval}
    recordhigh {set UnoRecordHigh $kval}
    recordfast {set UnoRecordFast $kval}
    recordcard {set UnoRecordCard $kval}
    recordwins {set UnoRecordWins $kval}
    recordplayed {set UnoRecordPlayed $kval}
   }
  }
  close $f
  if {$UnoStopAfter < 0} {set UnoStopAfter 0}
  if {$UnoBonus < 0} {set UnoBonus 1000}
  if {($UnoWildDrawTwos < 0)||($UnoWildDrawTwos > 1)} {set UnoWildDrawTwos 0}
  return
 }
 putcmdlog "\[Uno\] Config file $UnoCFGFile not found... saving defaults"
 Uno_WriteCFG
 return
}

#
# Write config file
#
proc Uno_WriteCFG {} {
 global UnoCFGFile UnoLastMonthCards UnoLastMonthGames UnoPointsName UnoScoreFile UnoRobot UnoChan UnoFast UnoHigh UnoPlayed UnoStopAfter UnoBonus
 global UnoRecordHigh UnoRecordFast UnoRecordCard UnoRecordWins UnoRecordPlayed UnoWildDrawTwos
 set f [open $UnoCFGFile w]
 puts $f "# This file is automatically overwritten"
 puts $f "BotName=$UnoRobot"
 puts $f "Channel=$UnoChan"
 puts $f "Points=$UnoPointsName"
 puts $f "ScoreFile=$UnoScoreFile"
 puts $f "StopAfter=$UnoStopAfter"
 puts $f "WildDrawTwos=$UnoWildDrawTwos"
 puts $f "LastMonthCard1=$UnoLastMonthCards(0)"
 puts $f "LastMonthCard2=$UnoLastMonthCards(1)"
 puts $f "LastMonthCard3=$UnoLastMonthCards(2)"
 puts $f "LastMonthWins1=$UnoLastMonthGames(0)"
 puts $f "LastMonthWins2=$UnoLastMonthGames(1)"
 puts $f "LastMonthWins3=$UnoLastMonthGames(2)"
 puts $f "Fast=$UnoFast"
 puts $f "High=$UnoHigh"
 puts $f "Played=$UnoPlayed"
 puts $f "Bonus=$UnoBonus"
 puts $f "RecordHigh=$UnoRecordHigh"
 puts $f "RecordFast=$UnoRecordFast"
 puts $f "RecordCard=$UnoRecordCard"
 puts $f "RecordWins=$UnoRecordWins"
 puts $f "RecordPlayed=$UnoRecordPlayed"
 close $f
 return
}

#
# Score advertiser
#
proc UnoScoreAdvertise {} {
 global UnoChan UnoAdNumber UnoRobot
 unomsg "0,4 UnoPoint - People's First Choice to Play UNO "
 switch $UnoAdNumber {
  0 {UnoTop10 0}
  1 {UnoLastMonthTop3 $UnoRobot none none $UnoChan 0}
  2 {UnoTop10 1}
  3 {UnoRecords $UnoRobot none none $UnoChan ""}
  4 {UnoPlayed $UnoRobot none none $UnoChan ""}
  5 {UnoHighScore $UnoRobot none none $UnoChan ""}
  6 {UnoTopFast $UnoRobot none none $UnoChan ""}
 }
 incr UnoAdNumber
 if {$UnoAdNumber > 6} {set UnoAdNumber 0}
 return
}

#
# Color all cards in hand
#
proc CardColorAll {cplayer} {
 global UnoHand
 set pCard ""
 set ccount 0
 while {[llength $UnoHand($cplayer)] != $ccount} {
  append pCard [CardColor [lindex $UnoHand($cplayer) $ccount]]
  incr ccount
 }
 return $pCard
}

#
# Color a single card
#
proc CardColor {pcard} {
  set cCard ""
  set c2 [string range $pcard 1 1]
  switch [string range $pcard 0 0] {
   "W" {
     if {$c2 == "D"} {
      append cCard "[wildf]"
     } {	
      append cCard "[wild]"
     }
     return $cCard
    }
   "Y" {append cCard " \0031,8 Yellow "}
   "R" {append cCard " \0030,4 Red "}
   "G" {append cCard " \0030,3 Green "}
   "B" {append cCard " \0030,12 Blue "}
  }
  switch $c2 {
   "S" {append cCard "\002Skip\002 \003 "}
   "R" {append cCard "\002Reverse\002 \003 "}
   "D" {append cCard "\002Draw Two\002 \003 "}
   default {append cCard "$c2 \003 "}
  }
  return $cCard
}

#
# Check if player has Uno
#
proc check_hasuno {cplayer} {
 global UnoHand
 if {[llength $UnoHand($cplayer)] > 1} {return}
 hasuno $cplayer
 return
}

#
# Check for winner
#
proc check_unowin {cplayer ccard} {
 global UnoHand
 if {[llength $UnoHand($cplayer)] > 0} {return 0}
 return 1
}

#
# Show player what cards they have
#
proc showcards {idx pcards} {
 global UnoIDX
 if {[uno_isrobot $idx]} {return}
 unontc [lindex $UnoIDX $idx] "Hand: $pcards"
}

#
# Check if this is the robot player
#
proc uno_isrobot {cplayerIDX} {
 global RoundRobin UnoRobot UnoMaxNickLen
 if {[string range [lindex $RoundRobin $cplayerIDX] 0 $UnoMaxNickLen] != $UnoRobot} {return 0}
 return 1
}

# Show played card
proc playcard {who crd nplayer} {
 unomsg "[nikclr $who]\003 Play's $crd \003to [nikclr $nplayer]\003"
}

# Show played draw card
proc playdraw {who crd dplayer nplayer} {
 unomsg "[nikclr $who]\003 plays $crd [nikclr $dplayer]\003 ,draw \0022 cards\002 in hand - continues with: [nikclr $nplayer]\003"
}

# Show played wildcard
proc playwild {who chooser} {
 unomsg "[nikclr $who]\003 play [wild] chooses a colour [nikclr $chooser]\003"
}

# Show played wild draw four
proc playwildfour {who skipper chooser} {
 unomsg "[nikclr $who]\003 plays [wildf] [nikclr $skipper]\003 ,draws \0024 cards\002 chooses a colour - continue with [nikclr $chooser]\003"
}

# Show played skip card
proc playskip {who crd skipper nplayer} {
 unomsg "[nikclr $who]\003 play $crd\003 and skipped [nikclr $skipper]\003 to [nikclr $nplayer]\003"
}

proc showwhodrew {who} {
 unomsg "[nikclr $who]\003 \002drew\002 a card"
}

proc playpass {who nplayer} {
 unomsg "[nikclr $who]\003 \002passes\002 to [nikclr $nplayer]\003"
}

proc botplaywild {who chooser ncolr nplayer} {
 unomsg "[nikclr $who]\003 plays [wild] and chooses $ncolr \003 current Player is: [nikclr $nplayer]\003"
}

# Show played wild draw four
proc botplaywildfour {who skipper chooser choice nplayer} {
 unomsg "[nikclr $who]\003 plays [wildf] [nikclr $skipper]\003 ,draws \0024 cards\002 choose a colour. [nikclr $chooser]\003 continue $choice\003 to player: [nikclr $nplayer]\003"
}

# Show a player what they drew
proc showdraw {idx crd} {
 global UnoIDX
 if {[uno_isrobot $idx]} {return}
 unontc [lindex $UnoIDX $idx] "Draw: $crd"
}

# Show Win 
proc showwin {who crd} {
 unomsg "[nikclr $who]\003 plays $crd \003und \002\00309W\00312I\00313N\00308S\002 [unoad]\003"
}

# Show Win by default
proc showwindefault {who} {
 unomsg "[nikclr $who] \002\00309W\00312I\00313N\00308S [unoad]\002\003 by default. \003"
}

# Player Has Uno
proc hasuno {who} {
 global UnoChan
 putquick "PRIVMSG $UnoChan :\001ACTION says [nikclr $who] \002\00309h\00312a\00313s \00309U\00312N\00313O\00308! \002\003\001"
}

#
# Utility Functions
#

# Check if a timer exists
proc unotimerexists {cmd} {
 foreach i [timers] {
  if {![string compare $cmd [lindex $i 1]]} then {
   return [lindex $i 2]
  }
 }
 return
}

# Sort Scores
proc UnoSortScores {s1 s2} {
 global unsortedscores
 if {$unsortedscores($s1) >  $unsortedscores($s2)} {return 1}
 if {$unsortedscores($s1) <  $unsortedscores($s2)} {return -1}
 if {$unsortedscores($s1) == $unsortedscores($s2)} {return 0}
}

# Calculate Game Running Time
proc game_time {} {
 global UnoStartTime
 set UnoCurrentTime [unixtime]
 set gt [expr ($UnoCurrentTime - $UnoStartTime)]
 return $gt
}

# Colorize Nickname
proc nikclr {nick} {
 global NickColor
 return "\003$NickColor($nick)$nick"
}
proc colornick {pnum} {
 global UnoNickColour
 set c [lindex $UnoNickColour [expr $pnum-1]]
 set nik [format "%02d" $c]
 return $nik
}

# Ratio Of Two Numbers
proc get_ratio {num den} {
 set n 0.0
 set d 0.0
 set n [expr $n +$num]
 set d [expr $d +$den]
 if {$d == 0} {return 0}
 set ratio [expr (($n /$d) *100.0)]
 return $ratio
}

# Name Of Last Month
proc UnoLastMonthName {month} {
 switch $month {
  00 {return "Dec"}
  01 {return "Jan"}
  02 {return "Feb"}
  03 {return "Mar"}
  04 {return "Apr"}
  05 {return "May"}
  06 {return "Jun"}
  07 {return "Jul"}
  08 {return "Aug"}
  09 {return "Sep"}
  10 {return "Oct"}
  11 {return "Nov"}
  default {return "???"}
 }
}

# String Pad
proc strpad {str len} {
 set slen [string length $str]
 if {$slen > $len} {return $str}
 while {$slen < $len} {
  append str " "
  incr slen
 }
 return $str
}

# Uno!
proc unoad {} {
 return "\002\0033U\00312N\00313O\00308!\002"
}

# Wild Card
proc wild {} {
 return " \0031,8 \002W\0030,3I \0030,4L\0030,12D\002 \003 "
}

# Wild Draw Four Card
proc wildf {} {
 return " \0031,8 \002W\0030,3I \0030,4L\0030,12D \0031,8D\0030,3r\0030,4a\0030,12w \0031,8F\0030,3o\0030,4u\0030,12r\002 \003 "
}

#
# Channel And DCC Messages
#

proc unomsg {what} {
 global UnoChan
 putquick "PRIVMSG $UnoChan :$what"
}

proc unontc {who what} {
 global UnoNTC
 putquick "$UnoNTC $who :$what"
}

proc unolog {who what} {
 putcmdlog "\[$who\] $what"
}

#
# DCC Routines
#

# Show All Players Cards
proc dccUnoHands {hand idx arg} {
 global UnoHand RoundRobin
 set n 0
 while {$n != [llength $RoundRobin]} {
  set un [lindex $RoundRobin $n]
  unolog $un $UnoHand($un)
  incr n
 }
}

# Rehash Configuration
proc dcc_unorehash {hand idx arg} {
 unolog "$hand" "Rehashing UNO Configuration"
 Uno_ReadCFG
 return
}

Uno_ReadCFG

UnoReadScores

putlog "Loaded Color Uno $UnoVersion (C) 2019 Powered By UnoPoint Team"
