' "Minenboot" game (Mine boat) for Sharp PC-1500 +4KB.
' © Happy Computer. 
' Author : Rupert Wagner
' Published in "Happy Computer" (January 1984).
' BASIC program.
' 
' The program can be started with "DEF A" or "RUN MINE". The aim of the game is to let your ship sail safely to the harbor on the right side of the display. Before the game starts, the computer hides up to seven mines on the game field. If the ship sails over a mine, it sinks and the game is over. The ship is equipped with a minesweeper to detect the mines. You can destroy mines with the ship front cannon, but do not destroy the search boat...
' When you reach the middle of the screen, you may be attacked by submarines shooting fast torpedoes. Torpedoes must be detroyed with the ship rear cannon.
' 
' Note: all controls are done through Inkey inputs and triggered by simple keystrokes; most controls have a repeat function.
' 
' Controls:
' * Reserve toggle: Toggle large/small boat control
' * RCL (Mine display): Visual display by bars or audible beeps. This key is effective only when the controller is on sweeper.
' * SPACE (has dual function): (a) control of sweeper. (b) Control of Ship.
' * Vertical scrolling keys: Set firing range. The firing range is in matrix points counted from the gun and displayed as a two-digit number. The corresponding gun must be loaded. 
' * ENTER (has two functions): (a) control of minesweeper. (b) control of ship firing.
' * Cursor Left or Cursor Right: Forward and backward search of boat or ship.
' * SML: Switch to rear cannon. The little cannon loads itself automatically, and can shoot automatic fire.
' (Rupert Wagner)
' 
' ---
' 
' "Minenboot" für den Sharp PC-1500
' Beim "Minenboot" handelt es sich um ein Spiel mit echter Display-Grafik. Man benötigt dafür mindestens eine 4-KByte-Speichererweiterung.
' 
' Das Programm kann mit "DEF A" oder "RUN MINE" gestartet werden. Ziel des Spiels ist es, das große Schiff sicher an die Hafenmauer auf der rechten Seite des Displays zu fahren. Wird das Suchboot während des Spiels nicht versenkt, so muß das Boot an die Hafenmauer und das Schiff direkt hinter das Boot gebracht werden. Vor Spielbeginn plaziert der Computer bis zu sieben unsichtbare Minen auf dem Spielfield. Diese Minen können nur vom Suchboot unbeschadet überfahren werden. 
' Läuft das Schiff auf eine dieser Minen, geht es unter und das Spiel ist beendet. Um diese Minen aber aufspüren zu können, ist das Suchboot mit einem Sensor-Rüssel ausgestattet. Die Anzeige der Minen erfolgt wahlweise mit Beep oder einem Balken variabler Höhe. Die Mitte des Suchrüssels ist genau dann über einer Mine, wenn der Balken sieben Matrixpunkte hoch ist, beziehungsweise wenn sieben schnelle Pieptöne hintereinander ertönen. Die Lage einer Mine kahn mit einer Boje markiert werden.
' Nun wird mit der vorderen Kanone des Schiffes so lange auf die Mine gerschossen, bis sie getroffen ist und explodiert. Dabei muß man aufpassen, daß man nicht das eigene Suchboot trifft und versenkt. Über das Suchboot kann jedoch ohne Bedenken hinweggeschossen werden. Hat sich das Schiff fast bis zur Mitte des Displays durchgekämpft, tritt eine weitere, wesentlich größere Gefahr hinzu. Es tauchen dann nämlich U-Boot-Schnorchel auf sowie Torpedos, die von Mal zu Mal schneller werden und das Schiff verfolgen. Diese Torpedos müssen mit der kleinen Heckkanone des Schiffes abgeschossen werden, bevor sie das Schiff versenken können.
' 
' Erreicht das Schiff trotz der Gefahren das Ziel, so werden die Materialverluste bewertet. Hierbei werden die vernichteten Minen und torpedos auf der Feindseite gegen die verschossenen großen und kleinen Granaten auf der eigenen Seite aufgewogen. Ein eventueller Verlust der Suchboots schlägt hier ebenfalls zu Buche.
' Alle Steuerfunktionen werden als Inkey-Funktionen durch (nicht zu kurze) Tastendrücke ausgelöst. Fast alle Steuertasten besitzen eine Repeat-Funktion!
' Folgende Steuerfunktionen stehen zur Verfügung:
' * Reserve-Ebenen-Taste: Umschalten der Steuerung von Schiff auf Boot und umgekehrt. Zur Kontrolle wird rechtsbündig auf dem Display ein großes oder kleines Boot angezeigt
' * RCL-Taste (Minenanzeige): Visuelle Anzeige durch Balken oder akustische Anzeige durch Beep. Zur Kontrolle wird rechtsbündig auf dem Display ein Balken mit "V" beziehungsweise ein Kopfhörersymbol angezeigt. Diese Taste ist nur wirksam, wenn die Steuerung auf Suchboot steht.
' * Space-Taste (besitzt Doppelfunktion): (a) Steuerung auf Suchboot: Hinter dem Boot wird eine Boje ausgesetzt, sofern der Abstand zum Schiff groß genug ist une sich nicht schon eine andere Boje auf dem Spielfeld befindet. (b) Steuerung auf Schiff: Die Bug-Kanone wird geladen. Zur kontrolle erscheint recht auf dem Display eine große Patrone. Der Ladevorgang muß vor jedem Schuß wiederholt werden.
' * Tasten für vertikales Durchrollen des Programmspeichers: Mit diesen beiden Tasten wird die Schußentfernung eingestellt. Die Schußentfernung wird in Matrixpunkten von der Kanonenmündung aus gezählt und als zweistellige Zahl angezeigt. Die entsprechende Kanone muß zum Einstellen geladen sein. Bei der Heck-Kanone erfolgt die Änderung immer in 5er-Sprüngen.
' * Enter-Taste (besitzt Doppelfunktion): (a) Steuerung auf Suchboot:abfrage der Minenanzeige für den jeweiligen Standort. (b) Steuerung auf Schiff:  Abfeuern der Bug-beziehungsweise Heck-Kanone.
' * Tasten für Cursor-Links beziehungsweise Cursor-Rechts: Vorwärts- und Rückwärtffahrt von Suchboot oder Schiff.
' * SML-Taste: Umschalten auf Heck-Kanone. Diese Umschaltung kann erst erfolgen, wenn fast die Steuerung auf "Schiff" steht. Die kleine Kanone lädt sich automatisch nach und kann auch Dauerfeuer schießen. Zur Kontrolle wird rechts auf dem Display eine Patrone angezeigt.
' 
' (Rupert Wagner)
'
5 "MINE"
10 "A"CLS :CLEAR :WAIT 90:PRINT "     * MINEN - BOOT *":RANDOM 
20 DIM B$(0)*56,A$(0)*22,C$(0)*24,D$(0)*18,E$(0)*38,Z$(7)*12
25 Z$(7)="000000000000",Z$(6)="404040404040",Z$(5)="606060606060"
26 Z$(4)="707070707070",Z$(3)="787878787878",Z$(2)="7C7C7C7C7C7C"
27 Z$(1)="7E7E7E7E7E7E",Z$(0)="7F7F7F7F7F7F"
30 B$(0)="085A6C4C5C4858485E4A5B4B5A4E5C4C5C485C7C8E4E4E2A1A0A02"
35 M$="003C223C0000",L$="407874784000",S$="4852641010645248"
40 A$(0)="1008140529762905140810",C$(0)="10305C545C58503818487840"
50 D$(0)="201010204020101020",E$="201825091E214E717E787E714E211E09251820"
55 F$="4C6C7C6040",N$="7C424141427C",O$="307E01017E30"
60 G$="40507060404040",H$="7070103838",I$="407E7D7D7E40"
70 A=0,C=0,N=1,I=900,J=900,K=30,L=10,UB$="",FI$="",U=0
80 D=40+RND 78,E=40+RND 35,F=40+RND 61,G=40+RND 67,H=40+RND 72,R=0,Y=80
90 IF G>80LET I=40+RND 39
100 IF H<80LET J=40+RND 70
105 CLS :WAIT 0
110 GCURSOR A:GPRINT B$(0):GCURSOR K:GPRINT C$(0):GCURSOR 120:GPRINT "7E7E":CURSOR 21:PRINT STR$ L;
120 M=9,N=1,X=0:GOTO 240
130 IF A>=55AND RND 70>20AND R=0LET OO=0:GOSUB 1030
131 IF R=1AND RND 30>5LET ZZ=0:GOSUB 1050
132 M=ASC INKEY$,ZZ=ZZ+1,OO=OO+1
135 IF M=0GOTO (132-(ZZ>=5)-(OO>=30))
140 IF M=11AND C=1AND L<94AND N=1WAIT 0:L=L+1+4*(P=0):CURSOR 21:PRINT "  ":CURSOR 21:PRINT STR$ L;:GOTO 130
150 IF M=10AND C=1AND L>10AND N=1WAIT 0:L=L-1-4*(P=0):CURSOR 21:PRINT "  ":CURSOR 21:PRINT STR$ L;:GOTO 130
160 IF M=12AND N=1AND A<YGOSUB 272:WAIT 0:GCURSOR ABS A:GPRINT "00":A=A+1:GCURSOR A:GPRINT B$(0);:GOTO 280
170 IF M=12AND N=0AND K<108AND X=0WAIT 0:GCURSOR K:GPRINT "00":K=K+1:GCURSOR K:GPRINT C$(0);:GOTO 130
178 IF M=13AND X=0AND N=0THEN 710
180 IF M=8AND N=0AND K>A+27AND X=0WAIT 0:GCURSOR (K+11):GPRINT "00":K=K-1:GCURSOR K:GPRINT C$(0);:GOTO 360
190 IF M=8AND N=1AND A>0WAIT 0:O=POINT (A+26):GCURSOR (A+26):GPRINT O-2*(O>=2);:A=A-1,MX=1
200 IF MX=1LET MX=0:GCURSOR A:GPRINT B$(0);:GOTO 360
210 IF M=13AND N=1AND C=1LET C=0,K$=@$(13+N),KG=KG+(P=1),KL=KL+(P=0):GOTO 398
220 IF M=2AND N=1AND A>=45LET C=1,P=0,K$=L$:WAIT 0:GCURSOR 147:GPRINT K$;:GOTO 130
225 IF M=32AND X=0AND N=0AND K>A+34AND XC=0LET U=K-5:BEEP 1,50,50:WAIT 0:GCURSOR U:GPRINT F$;:XC=1:GOTO 130
230 IF M=32AND N=1LET P=1,C=1,K$=I$:WAIT 0:GCURSOR 147:GPRINT K$;:GOTO 130
240 IF M=9AND N=1AND X=0LET N=0,C=0,K$=M$,P=1:GCURSOR 147:WAIT 0:GPRINT K$;:GOTO 130
250 IF M=9AND N=0LET N=1,C=0,K$=N$,P=1:GCURSOR 147:WAIT 0:GPRINT K$;:GOTO 130
260 IF M=25AND N=0AND X=0AND W=0LET K$=O$,W=1:WAIT 0:GCURSOR 147:GPRINT K$;:GOTO 130
265 IF M=25AND N=0AND X=0AND W=1LET W=0:WAIT 0:GCURSOR 147:GPRINT "787E78736443";:GOTO 130
270 GOTO 130
272 IF A>=U-27AND U>0LET A=A-1:RETURN
273 IF A>=K-27AND K>0LET A=A-1:RETURN
274 RETURN
280 FOR S=4TO 10:IF @(S)=A+23LET @(S)=900,T=1E6
290 NEXT S
300 IF T=1E6THEN 810
310 IF A=YTHEN 910
320 IF A+25>=KAND X=0LET X=1,Y=93,K=0
340 GOTO 130
350 BEEP 1,60,3:WAIT 0:GCURSOR U:GPRINT "0000000000":GCURSOR A:GPRINT B$(0);:U=0,XC=0:GOTO 130
360 IF K<U+5WAIT 0:GCURSOR U:XC=0:GPRINT "0000000000":GCURSOR A:GPRINT B$(0);:U=0
370 GOTO 130
380 IF A+1<=Z+7AND FI$=G$LET T=1E-9:GOTO820
390 GOTO 130
398 WAIT 0:BEEP 1,150,50:GCURSOR 147:GPRINT K$;
400 IF P=0AND A>=45THEN 600
410 SW=A+L+27:IF SW>=120LET SW=120
420 FOR S=A+29TO SW-3:HW=2+POINT S:GCURSOR S:GPRINT HW;:NEXT S
430 GCURSOR (SW-2):HW=POINT (SW-2)+4:GPRINT HW:GCURSOR (SW-1):HW=POINT (SW-1)+8
440 GPRINT HW:GCURSOR SW:HW=112+POINT SW:WAIT 30:GPRINT HW;
450 GOSUB 560
490 IF U>0AND SW>=U-1AND SW<=U+5WAIT 50:BEEP 2,150,30:U=0,XC=0,V=V+1:GCURSOR SW-3:GPRINT S$;:GOTO 790
510 WAIT 30:GCURSOR SW-4:BEEP 1,50,15:GPRINT D$(0):BEEP 1,70,20:GCURSOR SW-5:GPRINT A$(0);
520 HW=0,XM=0,ZM=0:FOR S=4TO 10:IF (SW>=@(S)-1)*(SW<=@(S)+1)LET @(S)=0,ZM=ZM+1,XM=1
525 IF ZM=1AND XM=1LET HW=1,V=V+1
527 XM=0
530 NEXT S
540 IF HW=1LET HW=0:BEEP 5,5,5:WAIT 50:GCURSOR SW-9:GPRINT E$(0);:GOTO 550
550 GCURSOR SW-5:WAIT 50:BEEP 6,5,7:GPRINT A$(0);
555 GOSUB 560:GOTO 130
560 WAIT 0:CLS :GCURSOR Z:GPRINT FI$:GCURSOR A:GPRINT B$(0)
570 IF U>0GCURSOR U:GPRINT F$
580 IF X=0GCURSOR K:GPRINT C$(0)
590 GCURSOR 120:GPRINT "7E7E":CURSOR 21:PRINT STR$ L:GCURSOR 147:GPRINT K$:RETURN
600 SW=A+1-L:IF SW<=9LET SW=9
610 WAIT 0:FOR S=A-1TO SW+5STEP -1
620 GCURSOR S:HW=1+POINT S:GPRINT HW:NEXT S
630 GCURSOR SW+4:HW=2+POINT (SW+4):GPRINT HW:GCURSOR SW+3:HW=4+POINT(SW+3):GPRINT HW
640 GCURSOR SW+2:HW=8+POINT (SW+2):GPRINT HW:GCURSOR SW+1:HW=16+POINT(SW+1):GPRINT HW
650 GCURSOR SW+2:HW=96+POINT SW:WAIT 30:GPRINT HW:GOSUB 560
660 IF SW>=ZAND SW<=Z+7THEN 690
670 WAIT 30:BEEP 1,50,15:GCURSOR SW-4:GPRINT D$(0)
680 GCURSOR SW-5:BEEP 1,70,20:GPRINT A$(0):GOSUB 560:WAIT 0:C=1,P=0:GCURSOR 147:GPRINT L$:GOTO 130
690 WAIT 30:BEEP 2,150,30:GCURSOR SW-3:GPRINT S$:BEEP 5,5,5:WAIT 50:GCURSOR SW-9:GPRINT E$(0)
700 TP=TP+1,R=0,FI$="":GOSUB 560:WAIT 0:C=1,P=0:GCURSOR 147:GPRINT L$;z=0:GOTO 130
710 IF W=1THEN 780
715 GOSUB 720:GOTO 770
720 HW=0
740 IF ABS (K+10-D)=HWOR ABS (K+10-E)=HWOR ABS (K+10-F)=HWOR ABS (K+10-G)=HWRETURN
750 IF ABS (K+10-H)=HWOR ABS (K+10-I)=HWOR ABS (K+10-J)=HWRETURN
760 HW=HW+1:IF HW<7THEN 740
765 HW=7:RETURN
770 WAIT 0:GCURSOR 147:GPRINT Z$(HW):GOTO 130
780 GOSUB 720:HW=7-HW:BEEP HW,5,50:GOTO 130
790 BEEP 5,30,10:GCURSOR K:WAIT 30:GPRINT "4040705070604060602060":Y=93
800 GCURSOR K:GPRINT D$(0):K=0:GOSUB 560:GOTO 130
810 GCURSOR (A+14):WAIT 60:BEEP 5,100,2:GPRINT E$(0):GCURSOR (A+27):WAIT 0:GPRINT "000000000000"
815 GCURSOR A:GPRINT B$(0):IF X=0GCURSOR K:GPRINT C$(0)
816 IF XC=1GCURSOR U:GPRINT F$
817 GOTO 835
820 GCURSOR (Z+9):WAIT 60:BEEP 5,100,2:GPRINT E$(0)
835 FOR S=2TO 6STEP 2:RESTORE (900+S):READ B$(0)
840 WAIT 0:CURSOR 21:PRINT "      "
850 WAIT 5:CURSOR 22:BEEP 3,20,150:PRINT "S";:BEEP 3,20,400:PRINT "O";:BEEP 3,20,150:PRINT "S"
860 WAIT 40:GCURSOR A:GPRINT B$(0);:NEXT S
870 WAIT 0:GCURSOR A+5:GPRINT "402020406010121161422042611112106040202040"
880 CURSOR 22:PRINT "   ":CURSOR 23:WAIT 120:GPRINT "4022223F222240":GOTO 1E3
902 DATA "206830307020602078286C2C683870307020703078383878687808"
904 DATA "002040404000000060203030206040404000404060606020202020"
906 DATA "000000000000000000004040000000000000000000000000000000"
910 BEEP 1,90,50:BEEP 1,70,50:BEEP 1,150,90:BEEP 1,150,100:BEEP 1,60,60:BEEP 1,200,200
920 WAIT 95:GCURSOR 147:GPRINT "183020657F65203018"
930 WAIT 95:CLS :PRINT "     Materialverluste";
931 WAIT 0:FOR MZ=15TO 139:MX=POINT MZ
933 MX=127-MX:GCURSOR MZ:BEEP 1,1,1:GPRINT ABS MX;:NEXT MZ
935 MX=127-POINT 140:GCURSOR 140:WAIT 80:GPRINT ABS MX:CLS 
940 WAIT 0:PRINT "FEIND:":CURSOR 10:GPRINT G$:CURSOR 12:PRINT TP
950 CURSOR 20:GPRINT "08085D222A225D0808":CURSOR 22:WAIT :PRINT V
960 WAIT 0:PRINT "EIGENE:":CURSOR 7:GPRINT C$(0):CURSOR 9:PRINT X
970 CURSOR 14:GPRINT I$:CURSOR 16:PRINT KG:CURSOR 21:GPRINT L$:CURSOR 23:WAIT :PRINT KL
975 CLS :S=(10*TP+5*V)/(20*X+2*KG+KL)
980 WAIT :IF S>1LET S=INT (10*S)/10:PRINT "FEINDVERLUSTE";S;"* HOEHER":GOTO 1E3
990 IF S<1LET S=1/S,S=INT (10*S)/10:PRINT "EIGENVERLUSTE";S;"* HOEHER"
1000 CLS :INPUT "noch ein Spiel ? (J/N)  ";U$:IF U$="J"THEN 10
1010 END
1030 R=1:WAIT 80:GCURSOR 5:GPRINT H$:WAIT 0:GCURSOR 5:GPRINT "0000000000";
1040 Z=RND 10+5:GCURSOR Z:GPRINT G$;:RETURN
1050 WAIT 0:GCURSOR Z:GPRINT "00000000000000":Z=Z+3+RND TP
1060 GCURSOR Z:GPRINT G$:FI$=G$
1070 IF A+1<=Z+7AND FI$=G$LET T=E-9:WAIT 0:GCURSOR Z:GPRINT "000000000000":GOTO 820
1080 RETURN 
