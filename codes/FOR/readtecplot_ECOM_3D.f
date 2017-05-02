      SUBROUTINE readtecplot_ECOM_3D(ISDITU)
    
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
      DIMENSION IJ_P(IJM),NU_IJES(300,6000,4),NUMIJS(9000000)
      CHARACTER*50 VERT,TMTEC,CMA,CIST,CIEND
      CHARACTER*150  IYTEC,FNAMERES6,FNAMERES7,FNAMERES8
      logical isexit
      INTEGER ISRCA
     
CW    =========输出tecplot可以看得文件==============      


      ELINI=0
      ZBMIN=50000
      IF(NUM3DS.GT.0)THEN
      DO i=1,NUM3DS  
       MA=NUM3DK(i)
        DO ITTEC=numtime1,numtime2 
            
         write(CMA,'(I3.3)')MA
         WRITE(TECIT,'(I5.5)')ITTEC
      
         OPEN(MA,FORM='formatted',
     &FILE=trim(FNAME(39))//"_"//trim(CMA) //"_"//TECIT//".DAT")    !gcmdm
         
         
          OPEN(143,FORM='formatted',
     &FILE=trim(FNAME(14))//"_"//trim(CMA)
     &//"_"//TECIT//".DAT")    !RCA的tecplot

         WRITE(*,*)"读取：",
     &trim(FNAME(39))//"_"//trim(CMA) //"_"//TECIT//".DAT"
 
          read(MA,*) VERT,VERT,IDT,VERT,TIME

          read(MA,*)VERT 

          read(MA,*)VERT,VERT,NU_KE,VERT,NU_IJE
          
          
          IF(ISECOMUV.EQ.1)THEN
             WRITE(143,*)'TITLE="EUV T=',TIME,'d"'

          WRITE(143,409)'VARIABLES="X ","Y ","ZB ","U ","V ","W "    
     &,"T " ,"IJ" ,"MA"          
     &'     
        
          WRITE(143,*)"ZONE N=",NU_KE," E=",NU_IJE," 
     &F=FEPOINT,ET=QUADRILATERAL "      
              
          END IF
          
          
          
          
      
            DO IK=1,NU_KE 
               read(MA,209)X0(1),Y0(1),(
     &U(1,k),
     &V(1,k),
     &T(1,K),W(1,K),L(1,K),K=1,KBM1)
     &,UA(1),VA(1),WU(1),WV(1),UTF(1),
     &(Q2(1,K),Q2L(1,K),AAMAX(1,K),AAMAY(1,K),
     &Q2B(1,K),Q2LB(1,K),K=1,KB-1),ZB(1),EL(1),ES(1),ED(1)
     &,ART(1),IJRL,IJ,MFS(1),MDU,MDV(1),MA
               
                   
               

             MKIJ(IK)=IJ

             X0(IJ)=X0(1)
             Y0(IJ)=Y0(1)
             DO K=1,KBM1
             U(IJ,k)=U(1,k)
             V(IJ,k)=V(1,k)
             T(IJ,K)=T(1,K)
             W(IJ,K)=W(1,K)
             L(IJ,K)=L(1,K)
             END DO

             UA(IJ)=UA(1)
             VA(IJ)=VA(1)
             WU(IJ)=WU(1)
             WV(IJ)=WV(1)
     
             DO K=1,KB-1
             Q2(IJ,K)=Q2(1,K)
             Q2L(IJ,K)=Q2L(1,K)
       
             Q2B(IJ,K)=Q2B(1,K)
             Q2LB(IJ,K)=Q2LB(1,K)
             END DO
             ZB(IJ)=ZB(1)
             EL(IJ)=EL(1)
             IF(ZB(IJ).LT.ZBMIN) ZBMIN=ZB(IJ)
             IF(EL(IJ).NE.0)ELINI=EL(IJ)
             
         
         IF(ISECOMUV.EQ.1)THEN
             WRITE(143,149)X0(1),Y0(1),173.,U(1,1),V(1,1)
     &,W(1,1),T(1,1),IJ,MA       
            END IF       
            END DO
            
            DO IM=1,NU_IJE
                READ(MA,*)(NU_IJES(MA,IM,IMS),IMS=1,4)
                
              IF(ISECOMUV.EQ.1)THEN
                WRITE(143,*)(NU_IJES(MA,IM,IMS),IMS=1,4)     
            END IF     
             END DO       
            
            
           CLOSE(MA)    
        
CW===========================输出二维地形结果==================================          

         
         
       IF(isditu.EQ.1)THEN 
          WRITE(143,*)'TITLE="ALL T=',TIME,'d"'

          WRITE(143,409)'VARIABLES="X ","Y ","ZB ","U ","V ","W "    
     &,"T " ,"IJ" ,"MA"          
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
          IF(ZB(IJ).GE.600) THEN  
              ZZZ=ELINI
          END IF
   
          
         WRITE(143,149)X0(IJ),Y0(IJ),ZZZ,0,0,0,0
     &,IJ,MA
149    FORMAT(7F18.3,5I10)   
       END DO
            
       DO IM=1,NU_IJE
         WRITE(143,*)(NU_IJES(MA,IM,IMS),IMS=1,4)
       END DO
       
      ELSE IF(isditu.EQ.2)THEN     !!!**********体三维**************      
          
      WRITE(143,*)'TITLE="ALL T=',TIME,'d"'       
       
        WRITE(143,409)'VARIABLES="X ","Y ","ZB ","U ","V ","W "    
     &,"T " ,"IJ" ,"MA"          
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
          IF(ZB(IJ).GE.600) THEN  
              ZZZ=ELINI
          END IF
   
          
         WRITE(143,149)X0(IJ),Y0(IJ),ZZZ,0,0,0,0
     &,IJ,MA
 
       END DO
       
        DO IK=1,NU_KE 
          IJ=MKIJ(IK) 
          TN=0.
          TP=0.
          COD=0.
          CHA=0.
          
          ZZZ=ZBMIN-100
   
          
         WRITE(143,149)X0(IJ),Y0(IJ),ZZZ,0,0,0,0
     &,IJ,MA
 
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
              IF(X0(IJ).GT.0.AND.ZB(IJ).LE.300.AND.ZB(IJ).GT.0)THEN
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
            IF(X0(IJ).GT.0.AND.X0(IJ1).GT.0.
     &AND.EL(IJ).GT.0.AND.EL(IJ1).GT.0.)THEN 
              NU_IJE=NU_IJE+1  
            END IF
            END DO
          END DO
       END DO          
       
    
          
       WRITE(143,*)'TITLE="3DS T=',TIME,'d"'

       WRITE(143,409)'VARIABLES="X ","Y ","ZB ","U ","V ","W "    
     &,"T " ,"IJ" ,"MA"          
     &'     

       WRITE(143,*)"ZONE N=",NU_KE," E=",NU_IJE," 
     &F=FEPOINT,ET=QUADRILATERAL " 
       
       DO J=1,NUM3DKIJ(I)
          DO IB=1,NUMZIJS(I,J)
            DO IY=1,KB
             IJ=IJ3DIJ(I,J,IB) 
             IF(X0(IJ).GT.0.AND.ZB(IJ).LE.300.AND.ZB(IJ).GT.0)THEN
          
            HHH=EL(IJ)-ZB(IJ)   
            ZZZ=ZB(IJ)+HHH*(KB-IY)/(KB-1) 
            
           IF(IY.GT.1)THEN
               T(IJ,IY)=T(IJ,IY)-IY/2
                
            END IF
             
            WRITE(143,149)X0(IJ),Y0(IJ),ZZZ,U(IJ,IY),V(IJ,IY),W(IJ,IY)
     &,T(IJ,IY)
     &,IJ,MA
            END IF  
            END DO
          END DO
       END DO
       
       DO J=1,NUM3DKIJ(I)
          DO IB=1,NUMZIJS(I,J)-1
            DO IY=1,KB-1
             IJ=IJ3DIJ(I,J,IB) 
             IJ1=IJ3DIJ(I,J,IB+1) 
            IF(X0(IJ).GT.0.AND.X0(IJ1).GT.0.
     &AND.EL(IJ).GT.0.AND.EL(IJ1).GT.0)THEN 
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
 409  FORMAT(A118)   
      
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

