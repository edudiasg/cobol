      *-----------------------------------------------------------------
      * PROGRAM....: EDUS0001
      * ANALIST....: C1276521 - EDUARDO DIAS GUSMAO
      * AUTHOR.....: C1276521 - EDUARDO DIAS GUSMAO
      * COMPILATION: Cobol 5.2
      * OBJECTIVE..: Metric query of similarity between two strings
      *              Implemented algorithm: Jaro Winkler Distance.
      *-----------------------------------------------------------------
      * VRS001 31.05.2023 - C1276521 - IMPLANTATION
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. EDUS0001.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book of parameters.
      *-----------------------------------------------------------------
       01 EDUS0001-RQSC.
          03 S0001E-TX-VRF                 PIC  X(255).
          03 S0001E-TX-FON                 PIC  X(255).

       01 EDUS0001-RPST.
          03 S0001S-VL-SML                 PIC S9(001)V9(2) COMP-3.
      *-----------------------------------------------------------------
      * Area de trabalho.
      *-----------------------------------------------------------------
       77 IC                           PIC  9(003)         VALUE ZEROS.
       77 IC2                          PIC  9(003)         VALUE ZEROS.
       77 IC3                          PIC  9(003)         VALUE ZEROS.
       77 DISTANCE-DSP                 PIC  9(009)         VALUE ZEROS.

       01 GRP-DST.
          03 GDA-CT-ESP                PIC S9(004) COMP    VALUE ZEROS.
          03 GDA-CT-LEN                PIC S9(004) COMP    VALUE ZEROS.
          03 WS-TX                     PIC  X(255)         VALUE SPACES.
          03 GDA-TX1                   PIC  X(255)         VALUE SPACES.
          03 GDA-TX2                   PIC  X(255)         VALUE SPACES.
          03 TX1-TAM                   PIC  9(003)         VALUE ZEROS.
          03 TX2-TAM                   PIC  9(003)         VALUE ZEROS.
          03 POS-INI                   PIC  9(003)         VALUE ZEROS.
          03 POS-FIM                   PIC  9(003)         VALUE ZEROS.
          03 DISTANCE-LIMIT            PIC  9(003)         VALUE ZEROS.
          03 SAME                      PIC  9(003)         VALUE ZEROS.
          03 TRANSP                    PIC  9(003)         VALUE ZEROS.
          03 DISTANCE                  PIC  9V9(8)         VALUE ZEROS.

       01 JARO-TAB.
          05 TAB-CONTENT               OCCURS 255.
             07 TX1-M                  PIC 9(1) VALUE 0.
                88 TX1-FIND-N          VALUE 0.
                88 TX1-FIND-S          VALUE 1.
             07 TX2-M                  PIC 9(1) VALUE 0.
                88 TX2-FIND-N          VALUE 0.
                88 TX2-FIND-S          VALUE 1.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-MAIN ROUTINE                SECTION.
      *-----------------------------------------------------------------

           PERFORM 100000-RECEIVE-REQUEST
           PERFORM 110000-VALIDA-PARAMETRO
           PERFORM 200000-PROCESSA
           PERFORM 400000-POSTA-RESPOSTA
           PERFORM 999999-ENCERRAR
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-RECEIVE-REQUEST            SECTION.
      *-----------------------------------------------------------------
           MOVE 001                         TO MSG1-CD-SEQL-ERROR

           INITIALIZE SV888-CTL SV888-RQSC SV888-RPST

           MOVE 'EDUS0001-RQSC'             TO SV888-NM-GET
           MOVE LENGTH OF EDUS0001-RQSC     TO SV888-LEN-GET

           PERFORM 883000-GET-CONTAINER

           IF SV888-CD-RTN NOT EQUAL 0
             MOVE 001                       TO MSG1-CD-ERROR-TS
             MOVE SV888-NM-PRG              TO MSG1-NM-PGM-ACND
             STRING
                'ERROR no GET CONTAINER ' SV888-TX-RTN
                        DELIMITED BY SIZE INTO MSG1-TX-ERROR-TS
             PERFORM 999000-GRAVA-ERROR-E-ENCERRA
           END-IF

           MOVE SV888-DADOS-SAIDA           TO EDUS0001-RQSC
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-VALIDA-PARAMETRO              SECTION.
      *-----------------------------------------------------------------
           MOVE 002                         TO MSG1-CD-SEQL-ERROR

           MOVE ZEROS                       TO S0001S-VL-SML

           IF  S0001E-TX-VRF EQUAL SPACES
               SET  ERROR-PROGRAMADO         TO TRUE
               MOVE 002                     TO MSG1-CD-ERROR-TS
               MOVE 003                     TO MSG1-CD-SEQL-ERROR
               MOVE 'Text of verification is invalid.'
                                            TO MSG1-TX-ERROR-TS
               PERFORM 999000-GRAVA-ERROR-E-ENCERRA
           END-IF

           IF  S0001E-TX-FON EQUAL SPACES
               SET  ERROR-PROGRAMADO         TO TRUE
               MOVE 003                     TO MSG1-CD-ERROR-TS
               MOVE 004                     TO MSG1-CD-SEQL-ERROR
               MOVE 'Text source is invalid.'
			                     TO MSG1-TX-ERROR-TS
               PERFORM 999000-GRAVA-ERROR-E-ENCERRA
           END-IF

           MOVE S0001E-TX-VRF               TO GDA-TX1
           MOVE S0001E-TX-FON               TO GDA-TX2
           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA                      SECTION.
      *-----------------------------------------------------------------
      * Carrega os tamanhos reais de cada string e processa comparativo.
      *-----------------------------------------------------------------
           MOVE 005                         TO MSG1-CD-SEQL-ERROR

           MOVE GDA-TX1                     TO WS-TX
           PERFORM 300000-RETIRA-ESP
           MOVE GDA-CT-LEN                  TO TX1-TAM

           MOVE GDA-TX2                     TO WS-TX
           PERFORM 300000-RETIRA-ESP
           MOVE GDA-CT-LEN                  TO TX2-TAM

           IF  TX1-TAM = ZEROS
           AND TX2-TAM = ZEROS
              MOVE 1                        TO DISTANCE
           ELSE
              PERFORM 210000-VRF-SIM
           END-IF

           MOVE DISTANCE                   TO S0001S-VL-SML
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-VRF-SIM                       SECTION.
      *-----------------------------------------------------------------
      * Verify the similarity metric between the two strings.
      * Jaro Winkler distance algorithm.
      *-----------------------------------------------------------------
           MOVE 006                         TO MSG1-CD-SEQL-ERROR

           COMPUTE DISTANCE-LIMIT =
                   FUNCTION MAX (TX1-TAM , TX2-TAM) / 2 - 1

           MOVE ZEROS                       TO SAME
           MOVE ZEROS                       TO TRANSP

           PERFORM VARYING IC FROM 1 BY 1
                     UNTIL IC > TX1-TAM

             COMPUTE POS-INI = FUNCTION MAX(1, IC - DISTANCE-LIMIT)

             COMPUTE POS-FIM = FUNCTION MIN(IC + DISTANCE-LIMIT,
                                            TX2-TAM )

              PERFORM VARYING IC2 FROM POS-INI BY 1
                        UNTIL IC2 > POS-FIM

                 IF TX2-FIND-S(IC2)
                 OR ( GDA-TX1(IC:1) NOT EQUAL GDA-TX2(IC2:1) )
                    CONTINUE
                 ELSE
                    SET TX1-FIND-S(IC)  TO TRUE
                    SET TX2-FIND-S(IC2) TO TRUE
                    ADD 1                   TO SAME
                    ADD POS-FIM             TO IC2
                 END-IF

              END-PERFORM

           END-PERFORM

           IF SAME = ZEROS
              MOVE SAME                    TO DISTANCE
           ELSE
              MOVE 1                        TO IC3

              PERFORM VARYING IC FROM 1 BY 1
                        UNTIL IC > TX1-TAM

                 IF NOT TX1-FIND-S(IC)
                    CONTINUE
                 ELSE

                    PERFORM UNTIL TX2-FIND-S(IC3)
                       ADD 1                TO IC3
                    END-PERFORM

                    IF GDA-TX1(IC:1) NOT EQUAL GDA-TX2(IC3:1)
                       ADD 1                TO TRANSP
                    END-IF

                    ADD 1                   TO IC3
                END-IF

              END-PERFORM

              COMPUTE DISTANCE = ((SAME / TX1-TAM) +
                                  ( SAME / TX2-TAM) +
                                  ((SAME - TRANSP / 2) /
                                    SAME)) / 3
           END-IF
           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-RETIRA-ESP                    SECTION.
      *-----------------------------------------------------------------
      * Remove the spaces of the string for the correct transference 
      * without loss.
      *-----------------------------------------------------------------
           MOVE 007                         TO MSG1-CD-SEQL-ERROR

           MOVE ZEROS                       TO GDA-CT-ESP
                                               GDA-CT-LEN

           INSPECT FUNCTION REVERSE ( WS-TX )
                   TALLYING GDA-CT-ESP FOR LEADING SPACE

           IF GDA-CT-ESP < LENGTH OF WS-TX
              SUBTRACT GDA-CT-ESP FROM LENGTH OF WS-TX
                       GIVING GDA-CT-LEN
           ELSE
              MOVE LENGTH OF WS-TX          TO GDA-CT-LEN
           END-IF
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-POSTA-RESPOSTA                SECTION.
      *-----------------------------------------------------------------
           MOVE 008                         TO MSG1-CD-SEQL-ERROR

           MOVE 'EDUS0001-RPST'             TO SV888-NM-PUT
           MOVE  LENGTH OF EDUS0001-RPST
                                            TO SV888-LEN-PUT

           MOVE SPACES                      TO SV888-DADOS-ENTRADA
           MOVE EDUS0001-RPST               TO SV888-DADOS-ENTRADA

           PERFORM 881000-PUT-CONTAINER

           IF SV888-CD-RTN NOT EQUAL ZEROS
              SET  ERROR-CICS                TO TRUE
              MOVE 004                      TO MSG1-CD-ERROR-TS
              MOVE 009                      TO MSG1-CD-SEQL-ERROR
              STRING 'ERROR no PUT container ' SV888-TX-RTN
                      DELIMITED BY SIZE INTO MSG1-TX-ERROR-TS
              END-STRING
              MOVE SV888-NM-PRG             TO MSG1-NM-PGM-ACND
              PERFORM 999000-GRAVA-ERROR-E-ENCERRA
           END-IF
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      * 881000-PUT-CONTAINER                SECTION.
      * 882000-LINK-IIBP2000                SECTION.
      * 883000-GET-CONTAINER                SECTION.
      * 884000-DESCOBRIR-CANAL              SECTION.
      * 885000-VARIAVEIS-MONITORADAS        SECTION.
      * 886000-EXCLUIR-CTNR-ERROR            SECTION.
      * 887000-EXCLUIR-PUT-CTNR             SECTION.
      * 888000-RECEBE-CONTAINER-ERROR        SECTION.
      *-----------------------------------------------------------------
-INC DELKP888
      *-----------------------------------------------------------------
       999000-GRAVA-ERROR-E-ENCERRA          SECTION.
      *-----------------------------------------------------------------

           MOVE 'MSG1-CD-ERROR-TS'           TO MSGKW999-NM-VRV-MNT
           MOVE  MSG1-CD-ERROR-TS            TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'MSG1-CD-SEQL-ERROR'         TO MSGKW999-NM-VRV-MNT
           MOVE  MSG1-CD-SEQL-ERROR          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'MSG1-TX-ERROR-TS'           TO MSGKW999-NM-VRV-MNT
           MOVE  MSG1-TX-ERROR-TS            TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'S0001E-TX-VRF'             TO MSGKW999-NM-VRV-MNT
           MOVE S0001E-TX-VRF               TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'S0001E-TX-FON'             TO MSGKW999-NM-VRV-MNT
           MOVE S0001E-TX-FON               TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE DISTANCE                   TO DISTANCE-DSP
           MOVE 'DISTANCE'                 TO MSGKW999-NM-VRV-MNT
           MOVE DISTANCE-DSP               TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERROR-TS             TO IIBKERROR-CD-ERROR
           MOVE MSG1-TX-ERROR-TS             TO IIBKERROR-TX-ERROR

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERROR'                   )
              CHANNEL   ( SV888-CANAL                   )
              FROM      ( IIBKERROR-PRM-ERROR           )
              FLENGTH   ( LENGTH OF IIBKERROR-PRM-ERROR )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      * 999001-TRATA-ABEND-CICS             SECTION.
      * 999002-TRATA-CONDITION-CICS         SECTION.
      * 999003-ADICIONA-VRV-MNT             SECTION.
      *-----------------------------------------------------------------
-INC HLPKP999
      *-----------------------------------------------------------------
      * MSG1-NTF-MSG                        SECTION.
      *-----------------------------------------------------------------
-INC MSGKPMG1
      *-----------------------------------------------------------------
       999999-ENCERRAR                      SECTION.
      *-----------------------------------------------------------------
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM EDUS0001 -----------------------------