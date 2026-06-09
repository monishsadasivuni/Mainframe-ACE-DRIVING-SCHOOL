000001        IDENTIFICATION DIVISION.                                 
000002        PROGRAM-ID. ACEREPT.                                     
000003        ENVIRONMENT DIVISION.                                    
000004        INPUT-OUTPUT SECTION.                                    
000005        FILE-CONTROL.                                            
000006             SELECT INST-FILE    ASSIGN TO INSTFILE              
000007                ORGANIZATION IS INDEXED                          
000008                ACCESS MODE IS SEQUENTIAL                        
000009                RECORD KEY IS INST-NUM                           
000010                FILE STATUS IS WS-INST-STATUS.                   
000011             SELECT STU-FILE     ASSIGN TO STUFILE               
000012                ORGANIZATION IS INDEXED                          
000013                ACCESS MODE IS SEQUENTIAL                        
000014                RECORD KEY IS STU-NUM                            
000015                FILE STATUS IS WS-STU-STATUS.                    
000016             SELECT REPORT-FILE  ASSIGN TO RPTFILE               
000017                ORGANIZATION IS SEQUENTIAL.      
000018        DATA DIVISION.                      
000019        FILE SECTION.                       
000020        FD  INST-FILE.                      
000021        01  INST-RECORD.                    
000022            05 INST-NUM      PIC 9(05).     
000023            05 INST-NAME     PIC X(20).     
000024            05 INST-SALARY   PIC 9(05).     
000025            05 INST-STATUS   PIC X(01).     
000026            05 FILLER        PIC X(49).     
000027        FD  STU-FILE.                       
000028        01  STU-RECORD.                     
000029           05 STU-NUM        PIC 9(05).     
000030           05 STU-NAME       PIC X(20).     
000031           05 STU-ADDRESS    PIC X(20).     
000032           05 STU-START-DATE PIC X(10).     
000033           05 STU-INST-NUM   PIC X(5).      
000034           05 STU-STATUS     PIC X(1).  
000035           05 STU-PF-DATE    PIC X(10).                             
000036           05 FILLER         PIC X(09).                             
000037        FD  REPORT-FILE                                             
000038            RECORDING MODE F                                        
000039            BLOCK CONTAINS 0 RECORDS                                
000040            RECORD CONTAINS 132 CHARACTERS.                         
000041        01  REPORT-LINE             PIC X(132).                     
000042        WORKING-STORAGE SECTION.                                    
000043        01  WS-INST-STATUS          PIC X(02) VALUE SPACES.         
000044        01  WS-STU-STATUS           PIC X(02) VALUE SPACES.         
000045        01  WS-COUNTERS.                                            
000046            05  WS-PAGE-NUM         PIC S9(05)  COMP-3 VALUE ZEROS. 
000047            05  WS-LINE-COUNT       PIC S9(05)  COMP-3 VALUE ZEROS. 
000048            05  WS-TOTAL-STU        PIC S9(07)  COMP-3 VALUE ZEROS. 
000049            05  WS-ACTIVE-STU       PIC S9(07)  COMP-3 VALUE ZEROS. 
000050            05  WS-PASSIVE-STU      PIC S9(07)  COMP-3 VALUE ZEROS. 
000051            05  WS-INST-COUNT       PIC S9(05)  COMP-3 VALUE ZEROS. 
000052        01  WS-COMP-FIELDS.                                          
000053            05  WS-STU-NUM-C        PIC S9(05)  COMP.                
000054            05  WS-INST-NUM-C       PIC S9(05)  COMP.                
000055        01  WS-MAX-LINES            PIC S9(05)  COMP-3 VALUE 15.     
000056        01  WS-LINES-USED           PIC S9(05)  COMP-3 VALUE 99.     
000057        01  WS-INST-TABLE.                                           
000058            05  WS-INST-ENTRY       OCCURS 100 TIMES                 
000059                                    INDEXED BY WS-INST-IDX.          
000060                10  WS-TAB-INST-NUM PIC 9(05)  COMP.                 
000061                10  WS-TAB-INST-NAM PIC X(20).                       
000062                10  WS-TAB-INST-SAL PIC 9(05).                       
000063                10  WS-TAB-INST-STS PIC X(01).                       
000064        01  WS-INST-MAX             PIC S9(05)  COMP-3 VALUE ZEROS.  
000065        01  WS-FOUND-INST-NAME      PIC X(20)  VALUE SPACES.         
000066        01  WS-DATE.                                                 
000067            05  WS-YEAR             PIC X(04).                       
000068            05  FILLER              PIC X(01).   
000069            05  WS-MONTH            PIC X(02).                 
000070            05  FILLER              PIC X(01).                 
000071            05  WS-DAY              PIC X(02).                 
000072        01  WS-RPT-DATE             PIC X(10).                 
000073        01  HDR-LINE-1.                                        
000074            05  FILLER              PIC X(01)  VALUE SPACES.   
000075            05  HDR-DATE            PIC X(10).                 
000076            05  FILLER              PIC X(05)  VALUE SPACES.   
000077            05  FILLER              PIC X(28)                  
000078                VALUE 'ACE DRIVING SCHOOL REPORT'.             
000079            05  FILLER              PIC X(20)  VALUE SPACES.   
000080            05  FILLER              PIC X(05)  VALUE 'PAGE:'.  
000081            05  HDR-PAGE            PIC Z,ZZ9.                 
000082            05  FILLER              PIC X(59)  VALUE SPACES.   
000083        01  HDR-LINE-2.                                        
000084            05  FILLER              PIC X(133) VALUE SPACES.   
000085        01  HDR-LINE-3.        
000086            05  FILLER              PIC X(01)  VALUE SPACES.        
000087            05  FILLER              PIC X(07)  VALUE 'STUDENT'.     
000088            05  FILLER              PIC X(05)  VALUE SPACES.        
000089            05  FILLER              PIC X(07)  VALUE 'STUDENT'.     
000090            05  FILLER              PIC X(13)  VALUE SPACES.        
000091            05  FILLER              PIC X(10)  VALUE 'INSTRUCTOR'.  
000092            05  FILLER              PIC X(08)  VALUE SPACES.        
000093            05  FILLER              PIC X(07)  VALUE 'STUDENT'.     
000094            05  FILLER              PIC X(76)  VALUE SPACES.        
000095        01  HDR-LINE-4.                                             
000096            05  FILLER              PIC X(01)  VALUE SPACES.        
000097            05  FILLER              PIC X(06)  VALUE 'NUMBER'.      
000098            05  FILLER              PIC X(06)  VALUE SPACES.        
000099            05  FILLER              PIC X(04)  VALUE 'NAME'.        
000100            05  FILLER              PIC X(14)  VALUE SPACES.        
000101            05  FILLER              PIC X(04)  VALUE 'NAME'.        
000102            05  FILLER              PIC X(14)  VALUE SPACES.   
000103            05  FILLER              PIC X(06)  VALUE 'STATUS'. 
000104            05  FILLER              PIC X(78)  VALUE SPACES.   
000105        01  HDR-LINE-5.                                        
000106            05  FILLER              PIC X(133) VALUE ALL '-'.  
000107        01  DETAIL-LINE.                                       
000108            05  FILLER              PIC X(01)  VALUE SPACES.   
000109            05  DL-STU-NUM          PIC Z9999.                 
000110            05  FILLER              PIC X(07)  VALUE SPACES.   
000111            05  DL-STU-NAME         PIC X(20).                 
000112            05  FILLER              PIC X(03)  VALUE SPACES.   
000113            05  DL-INST-NAME        PIC X(20).                 
000114            05  FILLER              PIC X(03)  VALUE SPACES.   
000115            05  DL-STATUS           PIC X(01).                 
000116            05  FILLER              PIC X(79)  VALUE SPACES.   
000117        01  TOTAL-LINE-1.                                      
000118            05  FILLER              PIC X(01)  VALUE SPACES.   
000119            05  FILLER              PIC X(25) 
000120                VALUE 'TOTAL STUDENTS PROCESSED:'.           
000121            05  TOT-STUDENTS        PIC ZZ,ZZ9.              
000122            05  FILLER              PIC X(101) VALUE SPACES. 
000123        01  TOTAL-LINE-2.                                    
000124            05  FILLER              PIC X(01)  VALUE SPACES. 
000125            05  FILLER              PIC X(25)                
000126                VALUE 'ACTIVE STUDENTS (C):'.                
000127            05  TOT-ACTIVE          PIC ZZ,ZZ9.              
000128            05  FILLER              PIC X(101) VALUE SPACES. 
000129        01  TOTAL-LINE-3.                                    
000130            05  FILLER              PIC X(01)  VALUE SPACES. 
000131            05  FILLER              PIC X(25)                
000132                VALUE 'PASSIVE STUDENTS (P/F):'.             
000133            05  TOT-PASSIVE         PIC ZZ,ZZ9.              
000134            05  FILLER              PIC X(101) VALUE SPACES. 
000135        01  WS-EOF-INST             PIC X(01) VALUE 'N'.     
000136        01  WS-EOF-STU              PIC X(01) VALUE 'N'.   
000137        PROCEDURE DIVISION.                                      
000138        0000-MAIN.                                               
000139            PERFORM 1000-INIT                                    
000140            PERFORM 2000-LOAD-INST-ARRAY                         
000141            PERFORM 3000-PROCESS-STUDENTS                        
000142                UNTIL WS-EOF-STU = 'Y'                           
000143            PERFORM 4000-PRINT-TOTALS                            
000144            PERFORM 5000-CLOSE                                   
000145            STOP RUN.                                            
000146        1000-INIT.                                               
000147            OPEN INPUT  INST-FILE                                
000148                 INPUT  STU-FILE                                 
000149                 OUTPUT REPORT-FILE                              
000150            MOVE FUNCTION CURRENT-DATE(1:4)  TO WS-YEAR          
000151            MOVE FUNCTION CURRENT-DATE(5:2)  TO WS-MONTH         
000152            MOVE FUNCTION CURRENT-DATE(7:2)  TO WS-DAY           
000153            STRING WS-MONTH '/' WS-DAY '/' WS-YEAR      
000154                DELIMITED SIZE INTO WS-RPT-DATE.                 
000155        2000-LOAD-INST-ARRAY.                                    
000156            READ INST-FILE                                       
000157                AT END MOVE 'Y' TO WS-EOF-INST                   
000158            END-READ                                             
000159            PERFORM UNTIL WS-EOF-INST = 'Y'                      
000160                COMPUTE WS-INST-MAX = WS-INST-MAX + 1            
000161                SET WS-INST-IDX TO WS-INST-MAX                   
000162                MOVE INST-NUM    TO WS-TAB-INST-NUM(WS-INST-IDX) 
000163                MOVE INST-NAME   TO WS-TAB-INST-NAM(WS-INST-IDX) 
000164                MOVE INST-SALARY TO WS-TAB-INST-SAL(WS-INST-IDX) 
000165                MOVE INST-STATUS TO WS-TAB-INST-STS(WS-INST-IDX) 
000166       *        INSPECT TO CLEAN UP INSTRUCTOR NAME              
000167                INSPECT WS-TAB-INST-NAM(WS-INST-IDX)             
000168                    CONVERTING LOW-VALUES TO SPACES              
000169                READ INST-FILE                                   
000170                    AT END MOVE 'Y' TO WS-EOF-INST  
000171                END-READ                                           
000172            END-PERFORM                                            
000173            CLOSE INST-FILE.                                       
000174        3000-PROCESS-STUDENTS.                                     
000175            READ STU-FILE                                          
000176                AT END MOVE 'Y' TO WS-EOF-STU                      
000177                NOT AT END PERFORM 3100-PROCESS-ONE-STUDENT        
000178            END-READ.                                              
000179        3100-PROCESS-ONE-STUDENT.                                  
000180       *    COMPUTE PAGE BREAK CHECK                               
000181            IF WS-LINES-USED >= WS-MAX-LINES                       
000182                PERFORM 3200-PRINT-HEADERS                         
000183            END-IF                                                 
000184            MOVE SPACES TO WS-FOUND-INST-NAME                      
000185            SET WS-INST-IDX TO 1                                   
000186            PERFORM VARYING WS-INST-IDX FROM 1 BY 1                
000187                UNTIL WS-INST-IDX > WS-INST-MAX         
000188                IF WS-TAB-INST-NUM(WS-INST-IDX) =               
000189                        FUNCTION NUMVAL(STU-INST-NUM)           
000190                    MOVE WS-TAB-INST-NAM(WS-INST-IDX)           
000191                        TO WS-FOUND-INST-NAME                   
000192                END-IF                                          
000193            END-PERFORM                                         
000194       *    CLEAN UP STUDENT NAME                               
000195            INSPECT STU-NAME                                    
000196                CONVERTING LOW-VALUES TO SPACES                 
000197            MOVE STU-NUM            TO DL-STU-NUM               
000198            MOVE STU-NAME           TO DL-STU-NAME              
000199            MOVE WS-FOUND-INST-NAME TO DL-INST-NAME             
000200            MOVE STU-STATUS         TO DL-STATUS                
000201            WRITE REPORT-LINE FROM DETAIL-LINE                  
000202                AFTER ADVANCING 1 LINE                          
000203            COMPUTE WS-LINES-USED  = WS-LINES-USED  + 1         
000204            COMPUTE WS-TOTAL-STU   = WS-TOTAL-STU   + 1  
000205            IF STU-STATUS = 'C'                                  
000206                COMPUTE WS-ACTIVE-STU  = WS-ACTIVE-STU  + 1      
000207            ELSE                                                 
000208                COMPUTE WS-PASSIVE-STU = WS-PASSIVE-STU + 1      
000209            END-IF.                                              
000210        3200-PRINT-HEADERS.                                      
000211            COMPUTE WS-PAGE-NUM   = WS-PAGE-NUM + 1              
000212            MOVE WS-RPT-DATE      TO HDR-DATE                    
000213            MOVE WS-PAGE-NUM      TO HDR-PAGE                    
000214            COMPUTE WS-LINES-USED = 0                            
000215            WRITE REPORT-LINE FROM HDR-LINE-1                    
000216                AFTER ADVANCING PAGE                             
000217            WRITE REPORT-LINE FROM HDR-LINE-2                    
000218                AFTER ADVANCING 1 LINE                           
000219            WRITE REPORT-LINE FROM HDR-LINE-3                    
000220                AFTER ADVANCING 1 LINE                           
000221            WRITE REPORT-LINE FROM HDR-LINE-4  
000222                AFTER ADVANCING 1 LINE               
000223            WRITE REPORT-LINE FROM HDR-LINE-5        
000224                AFTER ADVANCING 1 LINE.              
000225        4000-PRINT-TOTALS.                           
000226            MOVE WS-TOTAL-STU   TO TOT-STUDENTS      
000227            MOVE WS-ACTIVE-STU  TO TOT-ACTIVE        
000228            MOVE WS-PASSIVE-STU TO TOT-PASSIVE       
000229            WRITE REPORT-LINE FROM HDR-LINE-2        
000230                AFTER ADVANCING 2 LINES              
000231            WRITE REPORT-LINE FROM TOTAL-LINE-1      
000232                AFTER ADVANCING 1 LINE               
000233            WRITE REPORT-LINE FROM TOTAL-LINE-2      
000234                AFTER ADVANCING 1 LINE               
000235            WRITE REPORT-LINE FROM TOTAL-LINE-3      
000236                AFTER ADVANCING 1 LINE.              
000237        5000-CLOSE.                                  
000238            CLOSE STU-FILE  
000239                  REPORT-FILE.               
