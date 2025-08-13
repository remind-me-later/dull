' "Aterrissage" for Sharp PC-1500
' Author : Olivier Bournac
' Published in Hebdogiciel #28 (April 1984)
' BASIC program - 1908 bytes.
'
' CLOAD "ATERRISSAGE"
' RUN
'
' Jeu : 
' Vous devez faire aterrir un avion a partir d'une situation parametrable (altitude, vitesse, carburant).
' La vitesse d'arrivee doit etre inferieure a 100 et l'altitude inferieure a 5.
' La vitesse ne doit jamais etre inferieure a 60.
'
' Commandes:
' Manche: U I H K N M
' Ailettes: curseurs Haut/Bas
'
' ----- BASIC program -----------------------------------------
1 "ATERRISSAGE"
2 "Manche: U I H K N M
3 "Ailettes: curs.Haut/Bas
10 "A":CLS :CLEAR :WAIT 0:RESTORE :X=5
20 FOR I=1 TO 9:READ @$(I):NEXT I
25 PRINT " (1) Commandes   (2) Jeu"
27 ON VAL INKEY$ +1 GOTO 27,340
29 PAUSE "Valeurs de depart...":A=100,V=100,C=800
30 INPUT "Altitude (100) : ";A
40 INPUT "Vitesse (100) : ";V
50 INPUT "Carburant(<900) (800):";C:IF C<0 OR C>900 THEN 50
55 CLS :M=C
60 GCURSOR 21:GPRINT "7F":T=INT (M/40):FOR Z=0 TO T-1:GCURSOR Z:GPRINT "7F":NEXT Z:USING "#####"
70 GCURSOR 22:GPRINT "3E1C08":GCURSOR 62:GPRINT "1C3E7F3E1C":GCURSOR 104:GPRINT "081C3E":GCURSOR 107:GPRINT 127,0,0,0,127
80 TIME =0
90 Z$=INKEY$ 
100 IF Z$=""LET C=C-5: GOTO 190
110 IF Z$="U"LET C=C-10:V=V+V/10:A=A-V/20:X=X-1: GOTO 190
120 IF Z$="I"LET C=C-10:V=V+V/10:A=A-V/20:X=X+1: GOTO 190
130 IF Z$="N"LET C=C-15:V=V-V/10:A=A+V/20:X=X-1: GOTO 190
140 IF Z$="M"LET C=C-15:V=V-V/10:A=A+V/20:X=X+1: GOTO 190
150 IF Z$="H"LET C=C-10:X=X-1: GOTO 190
160 IF Z$="K"LET C=C-10:X=X+1: GOTO 190
170 IF Z$=CHR$ 10LET C=C-7:V=V-V*3/20:A=A-V/20: GOTO 190
180 IF Z$=CHR$ 11LET C=C+13:V=V+V/5:A=A+V/20
190 BEEP 1,20,10:IF X>9LET X=2
200 IF X<1LET X=8
210 GCURSOR 109:GPRINT @$(X)
220 IF C<=M-40LET M=M-40:GCURSOR M/40:GPRINT 0
230 GCURSOR 26:PRINT "A";A:GCURSOR 68:PRINT "V";V
240 IF (C<0)+(A<0)+(V>=2000)+(V<=60)>=1 THEN 270
250 IF A<=10IF X=5IF V<=100IF V>60 THEN 300
260  GOTO 90
270 GCURSOR 109:PRINT "*":BEEP 5,140,80
280 PAUSE "VOUS VOUS ETES ECRASE":USING 
290 WAIT 150:PRINT "C :";INT C;"  A :";INT A;"  V :";INT V:END
300 BEEP 10,10:USING :WAIT 150:PRINT "BRAVO! C:";INT C;" A:";INT A;" V:";INT V
310 PRINT "TEMPS :";USING "###.##";TIME *100:USING :END
320 DATA "0808180808","0214081020","00083E0000","2014080402","08080C0808"
330 DATA "0204081420","00003E0800","2010081402","0808180808"
340 PAUSE "Vous devez faire aterrir"
350 PAUSE "un avion a partir d"+CHR$ 34+"une"
360 PAUSE "situation parametrable."
370 PAUSE "(altit., vitesse, carbur.)"
372 PAUSE "La vitesse d"+CHR$ 34+"arrivee doit"
373 PAUSE "etre inferieure a "+CHR$ 34+"100"+CHR$ 34+".";:PAUSE 
374 PAUSE "Et l"+CHR$ 34+"altitude inf. a "+CHR$ 34+"5"+CHR$ 34+".";:PAUSE ""
377 PAUSE "La vitesse ne doit jamais"
378 PAUSE "etre inferieure a "+CHR$ 34+"60"+CHR$ 34+"."
380 PAUSE "Le manche a balai est..."
390 PAUSE "simule par: U I H K N M";:PAUSE "";:PAUSE 
400 WAIT 0:PRINT "Les ailettes par : ";:GPRINT "04027F020400000010207F2010";:PAUSE "";:PAUSE ""
410  GOTO 29
