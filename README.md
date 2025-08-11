# Dull

A BASIC to binary compiler for the Sharp PC-1500 series of computers.

## Programs that parse (unchecked run)

All original files are encoded in ISO-8859 and found in [Sharp PC-1500 (TRS-80 PC-2) resource page](http://www.pc1500.com/).

| Program                | Parses | Comments                                                               |
| ---------------------- | ------ | ---------------------------------------------------------------------- |
| atterisage             | [x]    |                                                                        |
| bataille-dans-l-espace | [x]    |                                                                        |
| bathyscap              | [x]    | '\\'' replaced by '^'                                                  |
| battlecars             | [ ]    | 3 letter identifiers                                                   |
| blackjack              | [x]    |                                                                        |
| bombing                | [x]    |                                                                        |
| bowling                | [x]    |                                                                        |
| course                 | [x]    |                                                                        |
| dames                  | [x]    |                                                                        |
| decathlon              | [x]    |                                                                        |
| donkey-kong            | [x]    |                                                                        |
| DungeonQuest           | [x]    |                                                                        |
| formula1               | [x]    |                                                                        |
| ghosthouse             | [x]    | 'GRPINT' at line 771 corrected                                         |
| gloupman               | [ ]    | Extra ',' at end of DATA                                               |
| invader                | [x]    |                                                                        |
| force                  | [ ]    | Extra ',' at ON GOTO, Tape commands: PRINT#                            |
| jackpot                | [x]    |                                                                        |
| jeu-des-blocks         | [?]    | Line 28 probably has an extra '"'                                      |
| labyrinthe             | [x]    |                                                                        |
| loup-des-mers          | [x]    |                                                                        |
| meteorites             | [x]    | '\\'' replaced by '^'                                                  |
| micromur               | [x]    |                                                                        |
| minenboot              | [x]    | Errors in bas file corrected from image listing: 'GCUROSR' at line 690 |
| mole                   | [x]    |                                                                        |
| monstres&merveilles    | [x]    | Strange S=[5D], lhasm syntax                                           |
| morpion                | [x]    |                                                                        |
| othello                | [x]    |                                                                        |
| pacman                 | [x]    |                                                                        |
| Pilesjr                | [x]    |                                                                        |
| rasemottes             | [x]    |                                                                        |
| scrabble               | [x]    |                                                                        |
| simulateur-de-vol      | [ ]    | Strange B=[5D], lhasm syntax                                           |
| slalom                 | [x]    |                                                                        |
| tank                   | [x]    |                                                                        |
| tempter                | [ ]    | IF without THEN clause in line 40                                      |
| trio                   | [x]    |                                                                        |

## Thanks

[Sharp PC-1500 (TRS-80 PC-2) resource page](http://www.pc1500.com/)
[Sharp_PC-1500_ROM_Disassembly](https://github.com/Jeff-Birt/Sharp_PC-1500_ROM_Disassembly)
[Sharp_CE-158](https://github.com/Jeff-Birt/Sharp_CE-158)
