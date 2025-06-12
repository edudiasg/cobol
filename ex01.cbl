        IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------        
        PROGRAM-ID. EX01.
      *-----------------------------------------------------------------        
        ENVIRONMENT DIVISION.
           DECIMAL-POINT IS PERIOD.
      *-----------------------------------------------------------------              
        DATA DIVISION.
      *-----------------------------------------------------------------
        WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------        
        01 FILE-INFO.
           03 FIRST-NAME  PIC  X(030)             VALUE SPACES.
           03 SALARY      PIC S9(009)V99 COMP-3   VALUE ZEROS.
        01 WS-END-OF-FILE PIC  X(001)             VALUE 'N'.
           88 END-OF-FILE                         VALUE 'S'.
        01 DISPLAY-SALARY PIC  -ZZZ,ZZZ,ZZZ.ZZ.
           
      *-----------------------------------------------------------------       
       FILE SECTION.
       FD FILE-NAME. 
        01 FD-FILE-INFO.              
           03 FD-FIRST-NAME PIC  X(030)           VALUE SPACES.
           03 FD-SALARY     PIC S9(009)V99 COMP-3 VALUE ZEROS.

       FD OUT-FILE-NAME. 
        01 OUT-FILE-INFO.              
           03 OUT-FIRST-NAME PIC  X(030)           VALUE SPACES.
           03 OUT-SALARY     PIC S9(009)V99 COMP-3 VALUE ZEROS.           
      *-----------------------------------------------------------------       
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       OPEN INPUT  FILE-NAME 
            OUTPUT OUT-FILE-NAME
       
       PERFORM READ-ROW
       
       PERFORM UNTIL END-OF-FILE
         
         COMPUTE SALARY = SALARY * 1.10
              ON SIZE ERROR DISPLAY 'ERROR TO COMPUTE, SALARY: ' SALARY 
              + ' FIRST-NAME: ' FIRST-NAME
         END-COMPUTE

         PERFORM WRITE-ROW

         MOVE SALARY TO DISPLAY-SALARY
         DISPLAY FIRST-NAME ' ' DISPLAY-SALARY

         PERFORM READ-ROW
       END-PERFORM

       CLOSE FILE-NAME 
             OUT-FILE-NAME

       STOP RUN.
      *-----------------------------------------------------------------
       READ-ROW SECTION.
      *-----------------------------------------------------------------      
       READ FILE-NAME INTO FILE-INFO
         AT END SET END-OF-FILE TO TRUE
       END-READ

       END-SECTION.    
      *-----------------------------------------------------------------
       WRITE-ROW SECTION.
      *-----------------------------------------------------------------      
        INITIALIZE OUT-FILE-INFO
           REPLACING ALPHANUMERIC DATA BY SPACES
                     NUMERIC DATA BY ZEROS

        MOVE FIRST-NAME TO OUT-FIRST-NAME  
        MOVE SALARY     TO OUT-SALARY

        WRITE OUT-FILE-INFO

       END-SECTION.
      *-----------------------------------------------------------------               