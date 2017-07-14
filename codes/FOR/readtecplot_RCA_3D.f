      SUBROUTINE readtecplot_RCA_3D(isditu) 
    
C     VERSION(08/07/91)
C
      INCLUDE 'INC\comdeck.INC'
      INCLUDE 'INC\ARCHIVE.INC'    
      INCLUDE 'INC\PARA.INC'
      INCLUDE 'INC\WVULP.INC'   
      INCLUDE 'INC\TRANSPORT.INC'  
      INCLUDE 'INC\ICNT.INC'      
      INCLUDE 'INC\INDEX.INC'      
      INCLUDE 'INC\ECOM3D.INC'      
      
      
      DIMENSION KE_P(IJM,KB),L_BOOK(IJM,4)
      DIMENSION CHALL(IJM,KB)

      DIMENSION TLIGHT1(IJM,KB),TEAT1(IJM,KB),TLOSS1(IJM,KB),
     &TEAT2(IJM,KB),TLOSS2(IJM,KB),
     &TEAT3(IJM,KB),TLOSS3(IJM,KB)

      DIMENSION IJ_P(IJM),NU_IJES(300,6000,4),NUMIJS(9000)
      CHARACTER*50 VERT,TMTEC,CMA,CIST,CIEND
      CHARACTER*150  IYTEC,FNAMERES6,FNAMERES7,FNAMERES8
      logical isexit
      INTEGER ISRCA
     
CW    =========输出tecplot可以看得文件==============      
      ZBMIN=50000
      
      IF(NUM3DS.GT.0)THEN
      DO i=1,NUM3DS  
       MA=NUM3DK(i)
        DO ITTEC=numtime1,numtime2 
          DO IY=1,KB-1
           write(CMA,'(I3.3)') MA          
           WRITE(IYTEC,'(I2.2)')IY
           WRITE(TECIT,'(I5.5)')ITTEC    
           OPEN(MA,FORM='formatted',
     &FILE=trim(FNAME(15))//"_"//trim(CMA)
     &//"_L"//trim(IYTEC)//"_"//TECIT//".DAT")    !RCA的tecplot
     
           WRITE(*,*)"读取：",
     &trim(FNAME(15))//"_"//trim(CMA)
     &//"_L"//trim(IYTEC)//"_"//TECIT//".DAT"
           READ(MA,*)VERT,VERT,TIME
           READ(MA,*)VERT
           READ(MA,*)VERT,VERT,NU_KE,VERT,NU_IJE
             DO IK=1,NU_KE
               READ(MA,109)X0(1),Y0(1),EL(1),ZB(1),
     & U(1,IY),
     & V(1,IY),  
     &T(1,IY),                        
     &(CARAY(1,IY,IS),IS=1,26),TN ,TP ,COD ,CHA,
     &TLIGHT1(1,IY) ,TEAT1(1,IY) ,TLOSS1(1,IY),
     &TEAT2(1,IY),TLOSS2(1,IY),
     &TEAT3(1,IY),TLOSS3(1,IY)
     &,IJRL,IJ,MA
               X0(IJ)=X0(1)
               Y0(IJ)=Y0(1)
               ZB(IJ)=ZB(1)
               
               IF(ZB(IJ).LT.ZBMIN) ZBMIN=ZB(IJ)
                   
                  
               
               EL(IJ)=EL(1)
               U(IJ,IY)=TN
               V(IJ,IY)=TP
               W(IJ,IY)=COD
               UF(IJ,IY)=CHA
               T(IJ,IY)=T(1,IY)
               TLIGHT1(IJ,IY)=TLIGHT1(1,IY)
               TEAT1(IJ,IY)=TEAT1(1,IY)
               TLOSS1(IJ,IY)=TLOSS1(1,IY)
               TEAT2(IJ,IY)=TEAT2(1,IY)
               TLOSS2(IJ,IY)=TLOSS2(1,IY)
               TEAT3(IJ,IY)=TEAT3(1,IY)
               TLOSS3(IJ,IY)=TLOSS3(1,IY)

               MKIJ(IK)=IJ
               CHALL(IJ,IY)=CHA
                DO IS=1,26
                   CARAY(IJ,IY,IS)=CARAY(1,IY,IS)
                END DO
             END DO
             
             DO IM=1,NU_IJE
                READ(MA,*)(NU_IJES(MA,IM,IMS),IMS=1,4)
             END DO  
            CLOSE(MA)
          END DO
          
CW===========================输出底部地形的二维结果==================================          
         OPEN(143,FORM='formatted',
     &FILE=trim(FNAME(7))//"_"//trim(CMA)
     &//"_"//TECIT//".DAT")    !RCA的tecplot

      IF(isditu.EQ.1)THEN   !!!***************面三维****************
          
          WRITE(143,*)'TITLE="ALL T=',TIME,'d"'

          WRITE(143,409)'VARIABLES="X ","Y ","ZB ","U ","V "  
     &,"T ","TN ","TP ","COD ","CHA "
     &,"LIGHT1 ","GRW1 ","LOSS1 "
     &,"GRW2 ","LOSS2 "
     &,"GRW3 ","LOSS3 "
     &,"IJ" ,"MA","ITM"          
     &'     
        
          WRITE(143,*)"ZONE N=",NU_KE," E=",NU_IJE," 
     &F=FEPOINT,ET=QUADRILATERAL " 
       
     
       DO IK=1,NU_KE 
          IJ=MKIJ(IK) 
          TN=0.
          TP=0.
          COD=0.
          CHA=0.
           
          ZZZ=ZB(IJ)
          IF(ZB(IJ).GT.EL(IJ)-10) THEN
              ZZZ=EL(IJ)
          END IF
         WRITE(143,149)X0(IJ),Y0(IJ),ZZZ,0,0,0
     &,TN,TP ,COD ,CHA 
     &,0,0,0
     &,0,0
     &,0,0
     &,IJ,MA,ITTEC
149    FORMAT(17F18.5,6I10)   
       END DO
            
       DO IM=1,NU_IJE
         WRITE(143,*)(NU_IJES(MA,IM,IMS),IMS=1,4)
       END DO
       
       
      ELSE IF(isditu.EQ.2)THEN     !!!**********体三维**************      
          
      WRITE(143,*)'TITLE="ALL T=',TIME,'d"'

          WRITE(143,409)'VARIABLES="X ","Y ","ZB ","U ","V "  
     &,"T ","TN ","TP ","COD ","CHA "
     &,"LIGHT1 ","GRW1 ","LOSS1 "
     &,"GRW2 ","LOSS2 "
     &,"GRW3 ","LOSS3 "
     &,"IJ" ,"MA","ITM"          
     &'     
        
          WRITE(143,*)"ZONE N=",NU_KE*2," E=",NU_IJE," 
     & ,DATAPACKING = POINT, ZONETYPE = FEBRICK "  
       
     
       DO IK=1,NU_KE    
          IJ=MKIJ(IK) 
          TN=0.
          TP=0.
          COD=0.
          CHA=0.
      
         ZZZ=ZB(IJ)    
        IF(ZB(IJ).GT.EL(IJ)-10) THEN
                ZZZ=EL(IJ)
        END IF
         WRITE(143,149)X0(IJ),Y0(IJ),ZZZ,0,0,0
     &,TN,TP ,COD ,CHA 
     &,0,0,0
     &,0,0
     &,0,0
     &,IJ,MA,ITTEC
  
      END DO
      
      DO IK=1,NU_KE    
          IJ=MKIJ(IK) 
          TN=0.
          TP=0.
          COD=0.
          CHA=0.
          ZZZ=ZBMIN-100
         WRITE(143,149)X0(IJ),Y0(IJ),ZZZ,0,0,0
     &,TN,TP ,COD ,CHA 
     &,0,0,0
     &,0,0
     &,0,0
     &,IJ,MA,ITTEC
  
      END DO     
      
            
       DO IM=1,NU_IJE
         WRITE(143,1433)(NU_IJES(MA,IM,IMS),IMS=1,4),
     *(NU_IJES(MA,IM,IMS)+NU_KE,IMS=1,4)
1433   FORMAT(8I10)
       END DO          
          

      END IF  
      
     
CW===========================输出三维结果===============     
   
       NU_KE=0
       NU_IJE=0
       DO J=1,NUM3DKIJ(I)
          DO IB=1,NUMZIJS(I,J)
            DO IY=1,KB
              IJ=IJ3DIJ(I,J,IB) 
              IF(X0(IJ).GT.0)THEN
               NU_KE=NU_KE+1  
               KE_P(IJ,IY)=NU_KE  
               IJ_P(NU_KE)=IJ 
              END IF
            END DO
          END DO
       END DO
       
        DO J=1,NUM3DKIJ(I)
          DO IB=1,NUMZIJS(I,J)-1
            DO IY=1,KB-1
            IJ=IJ3DIJ(I,J,IB) 
            IJ1=IJ3DIJ(I,J,IB+1) 
            IF(X0(IJ).GT.0.AND.X0(IJ1).GT.0)THEN 
              NU_IJE=NU_IJE+1  
            END IF
            END DO
          END DO
       END DO          
       
    
          
       WRITE(143,*)'TITLE="3DS T=',TIME,'d"'

       WRITE(143,409)'VARIABLES="X ","Y ","ZB ","U ","V ",
     &,"T ","TN ","TP ","COD ","CHA "
     &,"LIGHT1 ","GRW1 ","LOSS1 "
     &,"GRW2 ","LOSS2 "
     &,"GRW3 ","LOSS3 "
     &,"IJ " ,"MA ","ITM "          
     &'              
       WRITE(143,*)"ZONE N=",NU_KE," E=",NU_IJE," 
     &F=FEPOINT,ET=QUADRILATERAL " 
       
       DO J=1,NUM3DKIJ(I)
          DO IB=1,NUMZIJS(I,J)
            DO IY=1,KB
             IJ=IJ3DIJ(I,J,IB) 
             IF(X0(IJ).GT.0)THEN
             TP=CARAY(IJ,IY,5)+CARAY(IJ,IY,6)+CARAY(IJ,IY,7)
     &+CARAY(IJ,IY,8)+CARAY(IJ,IY,9) 
             TN=(CARAY(IJ,IY,10)+CARAY(IJ,IY,11))/0.89+CARAY(IJ,IY,12)
     &+CARAY(IJ,IY,13)+CARAY(IJ,IY,14)+CARAY(IJ,IY,15) 
            COD=CARAY(IJ,IY,18)+CARAY(IJ,IY,19)+CARAY(IJ,IY,20)
     &+CARAY(IJ,IY,21)+CARAY(IJ,IY,22)+CARAY(IJ,IY,23)+CARAY(IJ,IY,24)
      
             CHA=CHALL(IJ,IY) 
            
            HHH=EL(IJ)-ZB(IJ)   
            ZZZ=ZB(IJ)+HHH*(KB-IY)/(KB-1) 
            
  
            
            WRITE(143,149)X0(IJ),Y0(IJ),ZZZ,U(IJ,IY),V(IJ,IY),T(IJ,IY)
     &,TN,TP ,COD ,CHA,
     &TLIGHT1(IJ,IY) ,TEAT1(IJ,IY) ,TLOSS1(IJ,IY),
     &TEAT2(IJ,IY),TLOSS2(IJ,IY),
     &TEAT3(IJ,IY),TLOSS3(IJ,IY) 
     &,IJ,MA,ITTEC
            END IF  
            END DO
          END DO
       END DO
       
       DO J=1,NUM3DKIJ(I)
          DO IB=1,NUMZIJS(I,J)-1
            DO IY=1,KB-1
             IJ=IJ3DIJ(I,J,IB) 
             IJ1=IJ3DIJ(I,J,IB+1) 
            IF(X0(IJ).GT.0.AND.X0(IJ1).GT.0)THEN 
            WRITE(143,*) KE_P(IJ,IY),KE_P(IJ,IY+1)
     &,KE_P(IJ1,IY+1),KE_P(IJ1,IY)
            END IF   
            END DO
          END DO
       END DO      


       
       
        CLOSE(143)
        
        END DO
       END DO
      END IF
        
     
      
      
      
      

  
 
 209  FORMAT(2F18.2,65F14.5,6I10)   
 500  FORMAT(F18.2,70(I10,36F16.5))   
 600  FORMAT(F18.2,70(I10,69F16.5))   
 309  FORMAT(4I8)   
 409  FORMAT(A148)   
      
 327   FORMAT(I10,F10.4,A10,5A150)     
1191	 FORMAT(2I10,12F14.4) 
1192	 FORMAT(2I10,6F14.4) 
1193	 FORMAT(2I10,F14.6) 
1194	 FORMAT(I10,16F14.6) 
1195	 FORMAT(I10,5F14.4) 
1196	 FORMAT(I10,2F14.6) 
       
       
 1617  FORMAT(F8.2,I10,60F17.5)   
 109  FORMAT(2F18.2,42F17.5,3I10)   
   

      RETURN
      END

