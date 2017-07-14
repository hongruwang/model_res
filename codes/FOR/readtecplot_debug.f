      SUBROUTINE readtecplot_debug(ISRCA)
    
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
      
      
      DIMENSION KE_P(IJM),L_BOOK(IJM,4) 
      DIMENSION IJ_P(IJM),NU_IJES(30,60,4),NUMIJS(9000)
      CHARACTER*50 VERT,TMTEC,CMA,CIST,CIEND
      CHARACTER*150  IYTEC,FNAMERES6,FNAMERES7,FNAMERES8
      logical isexit
      INTEGER ISRCA
     

      IF(mmoni.GT.0)THEN
      DO i=1,mmoni  
       MA=NUMjS(i)
 
       write(CMA,'(I4.4)')MA
       FNAMEGEO=trim(FNAME(23))//"_"//trim(CMA)//".dat"  !gcm_dm输出文件序列
       write(CMA,'(I3.3)')MA
      
       
       OPEN(IUGRD,FILE=FNAMEGEO)
        READ(IUGRD,*)  
        READ(IUGRD,*) IKB 
C------------------- 读取sigma层中每层的分布值Z(K) ---------------------------
        DO 140 K=1,IKB
         READ(IUGRD,*) Z(K)
  140   CONTINUE
  
        READ(IUGRD,*) 
  
        READ(IUGRD,*)   IJNUM    !读取iix，ijy，网格数
    
        DO   N=1,IJNUM
         READ(IUGRD,121,END=102,ERR=102)
     .     IJ,H1P(IJ),H2P(IJ),ZB(IJ),HSIG(IJ),X0(IJ),
     . y0(IJ),X0(IJ),y0(IJ),X0(IJ),Y0(IJ),IJ0(IJ),
     &(IJ_LNP(IJ,IK),IK=1,4),I0(IJ),J0(IJ)
 121     FORMAT(I8,10F15.3,I15,6I10)           
        END DO
       
   
102    CLOSE(IUGRD)
       
       IF(ISRCA.EQ.1)THEN   !!!ecom结果的统计
          
           
       DO ITTEC=numtime1,numtime2
            
         WRITE(TECIT,'(I5.5)')ITTEC
      
         OPEN(MA,FORM='formatted',
     &FILE=trim(FNAME(39))//"_"//trim(CMA) //"_"//TECIT//".DAT")    !gcmdm

         WRITE(*,*)"读取：",
     &trim(FNAME(39))//"_"//trim(CMA) //"_"//TECIT//".DAT"
 
          read(MA,*) VERT,VERT,IDT,VERT,TIME

          read(MA,*)VERT 

          read(MA,*)VERT,VERT,NU_KE
      
            DO IK=1,NU_KE 
               read(MA,209)X0(1),Y0(1),(
     &U(1,k),
     &V(1,k),
     &T(1,K),W(1,K),L(1,K),K=1,KBM1)
     &,UA(1),VA(1),WU(1),WV(1),UTF(1),
     &(Q2(1,K),Q2L(1,K),AAMAX(1,K),AAMAY(1,K),
     &Q2B(1,K),Q2LB(1,K),K=1,KB-1),ZB(1),EL(1),ES(1),ED(1)
     &,ART(1),IJRL,IJ,MFS(1),MDU,MDV(1),MA



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
             
             ED(IJ)=MAX(0.,EL(IJ)-ZB(IJ))
             
             IJ1=IJ_LNP(IJ,1)
             IJ3=IJ_LNP(IJ,3)
             
             IF(IJ1.GT.0)THEN
             XMOM(IJ)=.25*(ED(IJ)+ED(IJ1))*(H2P(IJ)+H2P(IJ1))*UA(IJ)
                END IF
             
            IF(IJ3.GT.0)THEN
             YMOM(IJ)=.25*(ED(IJ)+ED(IJ3))*(H1P(IJ)+H1P(IJ3))*VA(IJ) 
              END IF
            
           END DO
           CLOSE(MA)     
           
           IF(ISQCHECK.EQ.1)THEN   !!!对ECOM的断面进行Q的统计 
            QAUEA=0.   
            QAVEA=0.   
           DO N=1,NUMIS(I)
             
                QAUEA=QAUEA+XMOM(IJMONI(I,N)) 
                QAVEA=QAVEA+YMOM(IJMONI(I,N)) 
             
           END DO
              
               
           WRITE(22000,2200)TIME,i,MA,NUMIS(1),QAUEA,QAVEA 
2200       Format(F8.3,2I5,10F18.3)           
          END IF
            
               
         WRITE(IMONI(i)+5000,500)TIME,(IJMONI(I,N),X0(IJMONI(I,N)),
     &Y0(IJMONI(I,N)),ZB(IJMONI(I,N)),EL(IJMONI(I,N)),
     &WU(IJMONI(I,N)),WV(IJMONI(I,N)),
     &(U(IJMONI(I,N),K),V(IJMONI(I,N),K),
     &W(IJMONI(I,N),K),T(IJMONI(I,N),K),Q2(IJMONI(I,N),K),
     &L(IJMONI(I,N),K),
     &K=1,KB-1),N=1,NUMIS(I))
         
     
         
       END DO
       END IF
       END DO
       
CW==================部分断面统计********END***************=============       
       
      IF(ISQCHEAVE.EQ.1)THEN  !!!!自动统计
       IJMONI=0
       IXU=0
       JXU=0
       NUMIS=0
      DO MA=MASTART,MAEND
       IMINX=800000
       JMINX=800000
       IMAXX=0
       JMAXX=0
          
       IF(IXU.EQ.0)THEN
           IXUFOR=1
       ELSE
           IXUFOR=IXU  
       END IF
       
       IFORE=1
       JXU=1
       write(CMA,'(I4.4)')MA
       FNAMEGEO=trim(FNAME(23))//"_"//trim(CMA)//".dat"  !gcm_dm输出文件序列
       
       write(CMA,'(I3.3)')MA
      
       
       OPEN(IUGRD,FILE=FNAMEGEO)
        READ(IUGRD,*)  
        READ(IUGRD,*) IKB 
C------------------- 读取sigma层中每层的分布值Z(K) ---------------------------
        DO   K=1,IKB
         READ(IUGRD,*) Z(K)
        END DO
  
        READ(IUGRD,*) 
  
        READ(IUGRD,*)   IJNUM    !读取iix，ijy，网格数
        
     
        
         DO N=1,IJNUM
         READ(IUGRD,121,END=1021,ERR=1021)
     .     IJ,H1P(IJ),H2P(IJ),ZB(IJ),HSIG(IJ),X0(IJ),
     . y0(IJ),X0(IJ),y0(IJ),X0(IJ),Y0(IJ),IJ0(IJ),
     &(IJ_LNP(IJ,IK),IK=1,4),I0(IJ),J0(IJ)
         
           I0(IJ)=I0(IJ)
           J0(IJ)=J0(IJ)
         
         
           IMINX=MIN(I0(IJ),IMINX)
           JMINX=MIN(J0(IJ),JMINX)
           IMAXX=MAX(I0(IJ),IMAXX)
           JMAXX=MAX(J0(IJ),JMAXX)
         
         
         !!!IF(IFORE.EQ.I0(IJ))THEN
         !!! IFORE=I0(IJ)
         !!! JXU=JXU+1
         !!!ELSE
         !!! IXU=IXU+1
         !!! JXU=1
         !!! IFORE=I0(IJ)
         !!!END IF
         
         
         
       
         IJMONJ(I0(IJ),J0(IJ))=IJ
            
         END DO
         

         
         
        
1021    CLOSE(IUGRD)
        
        
CW===============断面沿着I方向====================
      IF(MA.LE.23)THEN
         DO IMXS=IMINX,IMAXX
             IXU=IXU+1
             JXU=1
         DO JMXS=JMINX,JMAXX
            IF(IJMONJ(IMXS,JMXS).GT.0)THEN
             JXU=JXU+1    
             IJMONI(IXU,JXU)=IJMONJ(IMXS,JMXS)
             NUMIS(IXU)=JXU
            END IF
          END DO    
          END DO
          
       ELSE  
CW===============断面沿着J方向====================        
         DO JMXS=JMINX,JMAXX
             IXU=IXU+1
             JXU=1
         DO IMXS=IMINX,IMAXX
            IF(IJMONJ(IMXS,JMXS).GT.0)THEN
             JXU=JXU+1    
             IJMONI(IXU,JXU)=IJMONJ(IMXS,JMXS)
             NUMIS(IXU)=JXU
            END IF
         END DO    
         END DO         
        END IF 
       
       IF(ISRCA.EQ.1)THEN   !!!ecom结果的统计
          
           
       DO ITTEC=numtime1,numtime2
            
         WRITE(TECIT,'(I5.5)')ITTEC
      
         OPEN(MA,FORM='formatted',
     &FILE=trim(FNAME(39))//"_"//trim(CMA) //"_"//TECIT//".DAT")    !gcmdm

         WRITE(*,*)"读取：",
     &trim(FNAME(39))//"_"//trim(CMA) //"_"//TECIT//".DAT"
 
          read(MA,*) VERT,VERT,IDT,VERT,TIME

          read(MA,*)VERT 

          read(MA,*)VERT,VERT,NU_KE
      
            DO IK=1,NU_KE 
               read(MA,209)X0(1),Y0(1),(
     &U(1,k),
     &V(1,k),
     &T(1,K),W(1,K),L(1,K),K=1,KBM1)
     &,UA(1),VA(1),WU(1),WV(1),UTF(1),
     &(Q2(1,K),Q2L(1,K),AAMAX(1,K),AAMAY(1,K),
     &Q2B(1,K),Q2LB(1,K),K=1,KB-1),ZB(1),EL(1),ES(1),ED(1)
     &,ART(1),IJRL,IJ,MFS(1),MDU,MDV(1),MAS



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
             
             ED(IJ)=MAX(0.,EL(IJ)-ZB(IJ))
             
             IJ1=IJ_LNP(IJ,1)
             IJ3=IJ_LNP(IJ,3)
             
             IF(IJ1.GT.0)THEN
              XMOM(IJ)=.25*(ED(IJ)+ED(IJ1))*(H2P(IJ)+H2P(IJ1))*UA(IJ)
             END IF
             
             IF(IJ3.GT.0)THEN
              YMOM(IJ)=.25*(ED(IJ)+ED(IJ3))*(H1P(IJ)+H1P(IJ3))*VA(IJ) 
             END IF
          END DO
            
         CLOSE(MA) 
         
CW==================输入指定控制体的四条边====================
         IF(MA.EQ.25)THEN
             IJ=657
             IJ1=IJ_LNP(IJ,1)
             IJ2=IJ_LNP(IJ,2)
             IJ3=IJ_LNP(IJ,3)
             IJ4=IJ_LNP(IJ,4)
             
              Q2B=0
             
              DO K=1,KB-1
                 Q2B(IJ,1)= Q2B(IJ,1)+0.25*(H2P(IJ1)+H2P(IJ))*
     .   (ED(IJ1)+ED(IJ))*U(IJ,K)
                  
                 Q2B(IJ,2)=Q2B(IJ,2)+0.25*(H2P(IJ)+H2P(IJ2))*
     .  (ED(IJ)+ED(IJ2))* U(IJ2,K)
                 
                 Q2B(IJ,3)=0.25*(H1P(IJ3)+H1P(IJ))*(ED(IJ3)+ED(IJ))*
     .   V(IJ,K)
                
                 Q2B(IJ,4)=0.25*(H1P(IJ)+H1P(IJ4))*(ED(IJ)+ED(IJ4))*
     .   V(IJ4,K)
                 
              END DO
                 
            WRITE(222000,202)TIME,IJ,MA,H1P(IJ),H2P(IJ),(
     &U(IJ,k),
     &V(IJ,k),
     &W(IJ,K),K=1,KB-1),EL(IJ),ZB(IJ)
                  
            WRITE(222000,202)TIME,IJ1,MA,H1P(IJ1),H2P(IJ1),(
     &U(IJ1,k),
     &V(IJ1,k),
     &W(IJ1,K),K=1,KB-1),EL(IJ1),ZB(IJ1)
                      
            WRITE(222000,202)TIME,IJ2,MA,H1P(IJ2),H2P(IJ2),(
     &U(IJ2,k),
     &V(IJ2,k),
     &W(IJ2,K),K=1,KB-1),EL(IJ2),ZB(IJ2)
                  
            WRITE(222000,202)TIME,IJ3,MA,H1P(IJ3),H2P(IJ3),(
     &U(IJ3,k),
     &V(IJ3,k),
     &W(IJ3,K),K=1,KB-1),EL(IJ3),ZB(IJ3)
                 
                  
            WRITE(222000,202)TIME,IJ4,MA,H1P(IJ4),H2P(IJ4),(
     &U(IJ4,k),
     &V(IJ4,k),
     &W(IJ4,K),K=1,KB-1),EL(IJ4),ZB(IJ4)
              
              
               WRITE(222000,202)TIME,IJ,MA,Q2B(IJ,1),
     &Q2B(IJ,2),Q2B(IJ,3),Q2B(IJ,4),EL(IJ),ZB(IJ)
     &,UA(IJ),VA(IJ),YMOM(IJ),XMOM(IJ)
202    FORMAT(F8.2,2I10,100F15.6)              
         END IF


         IF(MA.EQ.26)THEN
             IJ=1190    !!!!25  657     26 1190
             IJ1=IJ_LNP(IJ,1)
             IJ2=IJ_LNP(IJ,2)
             IJ3=IJ_LNP(IJ,3)
             IJ4=IJ_LNP(IJ,4)
             
             
               Q2B=0
             
              DO K=1,KB-1
                 Q2B(IJ,1)= Q2B(IJ,1)+0.25*(H2P(IJ1)+H2P(IJ))*
     .   (ED(IJ1)+ED(IJ))*U(IJ,K)
                  
                 Q2B(IJ,2)=Q2B(IJ,2)+0.25*(H2P(IJ)+H2P(IJ2))*
     .  (ED(IJ)+ED(IJ2))* U(IJ2,K)
                 
                 Q2B(IJ,3)=0.25*(H1P(IJ3)+H1P(IJ))*(ED(IJ3)+ED(IJ))*
     .   V(IJ,K)
                
                 Q2B(IJ,4)=0.25*(H1P(IJ)+H1P(IJ4))*(ED(IJ)+ED(IJ4))*
     .   V(IJ4,K)
                 
              END DO
             
            WRITE(222000,202)TIME,IJ,MA,H1P(IJ),H2P(IJ),(
     &U(IJ,k),
     &V(IJ,k),
     &W(IJ,K),K=1,KB-1),EL(IJ),ZB(IJ)
                  
            WRITE(222000,202)TIME,IJ1,MA,H1P(IJ1),H2P(IJ1),(
     &U(IJ1,k),
     &V(IJ1,k),
     &W(IJ1,K),K=1,KB-1),EL(IJ1),ZB(IJ1)
                      
            WRITE(222000,202)TIME,IJ2,MA,H1P(IJ2),H2P(IJ2),(
     &U(IJ2,k),
     &V(IJ2,k),
     &W(IJ2,K),K=1,KB-1),EL(IJ2),ZB(IJ2)
                  
            WRITE(222000,202)TIME,IJ3,MA,H1P(IJ3),H2P(IJ3),(
     &U(IJ3,k),
     &V(IJ3,k),
     &W(IJ3,K),K=1,KB-1),EL(IJ3),ZB(IJ3)
                 
                  
            WRITE(222000,202)TIME,IJ4,MA,H1P(IJ4),H2P(IJ4),(
     &U(IJ4,k),
     &V(IJ4,k),
     &W(IJ4,K),K=1,KB-1),EL(IJ4),ZB(IJ4)
              
             
              
                WRITE(222000,202)TIME,IJ,MA,Q2B(IJ,1),
     &Q2B(IJ,2),Q2B(IJ,3),Q2B(IJ,4),EL(IJ),ZB(IJ)
     &,UA(IJ),VA(IJ),YMOM(IJ),XMOM(IJ)
             
         END IF

         
           
           DO I=IXUFOR,IXU !!!对ECOM的断面进行Q的统计 
            QAUEA=0.   
            QAVEA=0.   
            DO N=1,NUMIS(I)
             
             IF(IJMONI(I,N).GT.0)  THEN
               QAUEA=QAUEA+XMOM(IJMONI(I,N)) 
               QAVEA=QAVEA+YMOM(IJMONI(I,N)) 
             END IF
            END DO
            
             
           WRITE(22100,2200)TIME,i,MA,NUMIS(1),QAUEA,QAVEA
       
          END DO
            
               
         WRITE(IMONI(i)+5000,500)TIME,(IJMONI(I,N),X0(IJMONI(I,N)),
     &Y0(IJMONI(I,N)),ZB(IJMONI(I,N)),EL(IJMONI(I,N)),
     &UA(IJMONI(I,N)),VA(IJMONI(I,N)),
     &(U(IJMONI(I,N),K),V(IJMONI(I,N),K),
     &W(IJMONI(I,N),K),T(IJMONI(I,N),K),Q2(IJMONI(I,N),K),
     &L(IJMONI(I,N),K),
     &K=1,KB-1),N=1,NUMIS(I))
         
     
         
        END DO 
           
        END IF
      
        END DO 
           
      END IF
CW==================自动统计完成====================

      ELSE

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
           READ(MA,*)VERT,VERT,NU_KE 

             DO IK=1,NU_KE
               READ(MA,109)X0(1),Y0(1),ZB(1),EL(1),
     & U(1,IY),
     & V(1,IY),  
     &T(1,IY),                        
     &(CARAY(1,IY,IS),IS=1,26),TN ,TP ,COD ,CHA,
     &TLIGHT1 ,TEAT1 ,TLOSS1,
     &TEAT2 ,TLOSS2,
     &TEAT3 ,TLOSS3
     &,IJRL,IJ,MA
       
               X0(IJ)=X0(1)
               Y0(IJ)=Y0(1)
               ZB(IJ)=ZB(1)
               EL(IJ)=EL(1)
      
               U(IJ,IY)=TN
               V(IJ,IY)=TP
               W(IJ,IY)=COD
               UF(IJ,IY)=CHA


               T(IJ,IY)=T(1,IY)


               L(IJ,IY)=TLIGHT1
               Q2L(IJ,IY)=TEAT1
               Q2LB(IJ,IY)=TLOSS1
              
          
               Q2L2(IJ,IY)=TEAT2
               Q2LB2(IJ,IY)=TLOSS2


            
               Q2L3(IJ,IY)=TEAT3
               Q2LB3(IJ,IY)=TLOSS3

                DO IS=1,26
                   CARAY(IJ,IY,IS)=CARAY(1,IY,IS)
                END DO
   
             END DO
            CLOSE(MA)
         END DO
         WRITE(IMONI(i)+5000,600)TIME,(IJMONI(I,N),X0(IJMONI(I,N)),
     &Y0(IJMONI(I,N)),ZB(IJMONI(I,N)),EL(IJMONI(I,N)),
     &(U(IJMONI(I,N),K),V(IJMONI(I,N),K),W(IJMONI(I,N),K),
     &CARAY(IJMONI(I,N),K,14),
     &UF(IJMONI(I,N),K),T(IJMONI(I,N),K),
     &CARAY(IJMONI(I,N),K,2),
     &CARAY(IJMONI(I,N),K,3),
     &CARAY(IJMONI(I,N),K,4),
     &CARAY(IJMONI(I,N),K,26),
     &L(IJMONI(I,N),K),Q2L(IJMONI(I,N),K),Q2LB(IJMONI(I,N),K),
     &Q2L2(IJMONI(I,N),K),Q2LB2(IJMONI(I,N),K),
     &Q2L3(IJMONI(I,N),K),Q2LB3(IJMONI(I,N),K),
     &K=1,KB-1),N=1,NUMIS(I))
      
       END DO  
       
     
      END IF
      
      
      
      
CW==============将分块输出结果整成一块=================      
       IF(ISTOALL.EQ.1)THEN
           
           
        IF(ISRCA.EQ.2)THEN  !!!!整理RCA结果
       
        DO ITTEC=numtime1,numtime2
            
         DO IY=1,KB-1
       
           WRITE(IYTEC,'(I2.2)')IY
           WRITE(TECIT,'(I5.5)')ITTEC 
           
           
           WRITE(CIST,'(I3.3)')MASTART    
           WRITE(CIEND,'(I3.3)')MAEND    
 
     
       OPEN(143,FORM='formatted',
     &FILE=trim(FNAME(47))//"_"//trim(CIST)//"_"//trim(CIEND)
     &//"_L"//trim(IYTEC)//"_"//TECIT//".DAT")    !RCA的tecplot 
          
 
          
         NU_KETALL=0
         NU_IJEALL=0
         
         
          DO IMA=MASTART,MAEND
           write(CMA,'(I3.3)')IMA
           OPEN(IMA,FORM='formatted',
     &FILE=trim(FNAME(15))//"_"//trim(CMA)
     &//"_L"//trim(IYTEC)//"_"//TECIT//".DAT")    !RCA的tecplot  

           READ(IMA,*)VERT,VERT,TIME
           READ(IMA,*)VERT
          read(IMA,*)VERT,VERT,NU_KE,VERT,NU_IJE
           NU_KETALL=NU_KETALL+NU_KE
           
          NU_IJEALL=NU_IJEALL+NU_IJE
         CLOSE(IMA)     
        END DO
     
        
     
       
       WRITE(143,*)'TITLE="ALL T=',TIME,'d"'

       WRITE(143,409)'VARIABLES="X ","Y ","EL ","ZB ","U ","V "
     @,"T " 
     &,"SAL ","PHYT1 ","PHYT2 ","PHYT3 ","RPOP "
     &,"LPOP ","RDOP ","LDOP ","PO4T ","RPON "
     &,"LPON ","RDON ","LDON ","NH4T ","NO23 "
     &,"BSI ","SIT ","RPOC ","LPOC ","RDOC "
     &,"LDOC ","EXDOC ","REPOC ","REDOC ","O2EQ "
     &,"DO ","TN ","TP ","COD ","CHA "
     &,"LIGHT1 ","GRW1 ","LOSS1 "
     &,"GRW2 ","LOSS2 "
     &,"GRW3 ","LOSS3 "
     &,"IJA ","IJ" ,"MA"          
     &'     
        
        WRITE(143,*)"ZONE N=",NU_KETALL," E=",NU_IJEALL," 
     &F=FEPOINT,ET=QUADRILATERAL " 
       
       
       
         NU_KETALL=0
         NU_IJEALL=0
         
         
          DO IMA=MASTART,MAEND
               
           write(CMA,'(I3.3)')IMA
           
           OPEN(IMA,FORM='formatted',
     &FILE=trim(FNAME(15))//"_"//trim(CMA)
     &//"_L"//trim(IYTEC)//"_"//TECIT//".DAT")    !RCA的tecplot  

         WRITE(*,*)"读取：",
     &trim(FNAME(15))//"_"//trim(CMA)
     &//"_L"//trim(IYTEC)//"_"//TECIT//".DAT"    !RCA的tecplot  
 
           READ(IMA,*)VERT,VERT,TIME

           READ(IMA,*)VERT
          
          read(IMA,*)VERT,VERT,NU_KE,VERT,NU_IJE
          
          
          DO IK=1,NU_KE 
               read(IMA,109)X0(1),Y0(1),ZB(1),EL(1),
     & U(1,IY),
     & V(1,IY),  
     &T(1,IY),                        
     &(CARAY(1,IY,IS),IS=1,26),TN ,TP ,COD ,CHA,
     &TLIGHT1 ,TEAT1 ,TLOSS1,
     &TEAT2 ,TLOSS2,
     &TEAT3 ,TLOSS3
     &,IJRL,IJ,MA
               
         IF(TEAT.NE.TEAT)TEAT=0     
         IF(TLIGHT.NE.TLIGHT)TLIGHT=0     
         IF(TLOSS.NE.TLOSS)TLOSS=0     
               
         WRITE(143,109)X0(1),Y0(1),ZB(1),EL(1),
     & U(1,IY),
     & V(1,IY),  
     &T(1,IY),                        
     &(CARAY(1,IY,IS),IS=1,26),TN ,TP ,COD ,CHA,
     &TLIGHT1 ,TEAT1 ,TLOSS1,
     &TEAT2 ,TLOSS2,
     &TEAT3 ,TLOSS3
     &,IJRL,IJ,MA
         END DO
            
         DO IM=1,NU_IJE
                READ(IMA,*)(NU_IJES(IMA,IM,IMS),IMS=1,4)
                IF(NU_KETALL.GT.0)THEN
                 DO IMS=1,4
                   NU_IJES(IMA,IM,IMS)=NU_IJES(IMA,IM,IMS)+NU_KETALL
                 END DO
                END IF
         END DO
         
         
          NU_KETALL=NU_KETALL+NU_KE
            
          NUMIJS(IMA)=NU_IJE
          NU_IJEALL=NU_IJEALL+NU_IJE
         CLOSE(IMA)     
        END DO
        
        DO IMA=MASTART,MAEND
          DO IM=1,NUMIJS(IMA)  
           WRITE(143,309)(NU_IJES(IMA,IM,IMS),IMS=1,4)
          END DO
        END DO  
        
        CLOSE(143)
        
        END DO
        END DO
        END IF
        
        
        
        
      IF(ISRCA.EQ.4)THEN  !!!!整理4漂浮物结果
       
       DO ITTEC=numtime1,numtime2
            
        DO IY=1,KB-1
       
           WRITE(IYTEC,'(I2.2)')IY
           WRITE(TECIT,'(I5.5)')ITTEC 
           
           
           WRITE(CIST,'(I3.3)')MASTART    
           WRITE(CIEND,'(I3.3)')MAEND    
 
     
       OPEN(143,FORM='formatted',
     &FILE=trim(FNAME(47))//"_"//trim(CIST)//"_"//trim(CIEND)
     &//"_L"//trim(IYTEC)//"_"//TECIT//".DAT")    !RCA的tecplot 
          
 
          
         NU_KETALL=0
         NU_IJEALL=0
         
         
          DO IMA=MASTART,MAEND
           write(CMA,'(I3.3)')IMA
           OPEN(IMA,FORM='formatted',
     &FILE=trim(FNAME(15))//"_"//trim(CMA)
     &//"_L"//trim(IYTEC)//"_"//TECIT//".DAT")    !RCA的tecplot  

           READ(IMA,*)VERT,VERT,TIME
           READ(IMA,*)VERT
          read(IMA,*)VERT,VERT,NU_KE,VERT,NU_IJE
           NU_KETALL=NU_KETALL+NU_KE
           
          NU_IJEALL=NU_IJEALL+NU_IJE
         CLOSE(IMA)     
        END DO
     
        
     
       
       WRITE(143,*)'TITLE="ALL T=',TIME,'d"'

       WRITE(143,409)'VARIABLES="X ","Y ","EL ","ZB ","U ","V "
     @,"T " 
     &,"SAL ","PHYT1 ","PHYT2 ","PHYT3 ","RPOP "
     &,"LPOP ","RDOP ","LDOP ","PO4T ","RPON "
     &,"LPON ","RDON ","LDON ","NH4T ","NO23 "
     &,"BSI ","SIT ","RPOC ","LPOC ","RDOC "
     &,"LDOC ","EXDOC ","REPOC ","REDOC ","O2EQ "
     &,"DO ","TN ","TP ","COD ","CHA "
     &,"LIGHT1 ","GRW1 ","LOSS1 "
     &,"GRW2 ","LOSS2 "
     &,"GRW3 ","LOSS3 "
     &,"IJA ","IJ" ,"MA"          
     &'     
        
        WRITE(143,*)"ZONE N=",NU_KETALL," E=",NU_IJEALL," 
     &F=FEPOINT,ET=QUADRILATERAL " 
       
       
       
         NU_KETALL=0
         NU_IJEALL=0
         
         
          DO IMA=MASTART,MAEND
               
           write(CMA,'(I3.3)')IMA
           
           OPEN(IMA,FORM='formatted',
     &FILE=trim(FNAME(15))//"_"//trim(CMA)
     &//"_L"//trim(IYTEC)//"_"//TECIT//".DAT")    !RCA的tecplot  

         WRITE(*,*)"读取：",
     &trim(FNAME(15))//"_"//trim(CMA)
     &//"_L"//trim(IYTEC)//"_"//TECIT//".DAT"    !RCA的tecplot  
 
           READ(IMA,*)VERT,VERT,TIME

           READ(IMA,*)VERT
          
          read(IMA,*)VERT,VERT,NU_KE,VERT,NU_IJE
          
          
          DO IK=1,NU_KE 
               read(IMA,109)X0(1),Y0(1),ZB(1),EL(1),
     & U(1,IY),
     & V(1,IY),  
     &T(1,IY),                        
     &(CARAY(1,IY,IS),IS=1,26),TN ,TP ,COD ,CHA,
     &TLIGHT1 ,TEAT1 ,TLOSS1,
     &TEAT2 ,TLOSS2,
     &TEAT3 ,TLOSS3
     &,IJRL,IJ,MA
               
         WRITE(143,109)X0(1),Y0(1),ZB(1),EL(1),
     & U(1,IY),
     & V(1,IY),  
     &T(1,IY),                        
     &(CARAY(1,IY,IS),IS=1,26),TN ,TP ,COD ,CHA,
     &TLIGHT1 ,TEAT1 ,TLOSS1,
     &TEAT2 ,TLOSS2,
     &TEAT3 ,TLOSS3
     &,IJRL,IJ,MA
         END DO
            
         DO IM=1,NU_IJE
                READ(IMA,*)(NU_IJES(IMA,IM,IMS),IMS=1,4)
                IF(NU_KETALL.GT.0)THEN
                 DO IMS=1,4
                   NU_IJES(IMA,IM,IMS)=NU_IJES(IMA,IM,IMS)+NU_KETALL
                 END DO
                END IF
         END DO
         
         
          NU_KETALL=NU_KETALL+NU_KE
            
          NUMIJS(IMA)=NU_IJE
          NU_IJEALL=NU_IJEALL+NU_IJE
         CLOSE(IMA)     
        END DO
        
        DO IMA=MASTART,MAEND
          DO IM=1,NUMIJS(IMA)  
           WRITE(143,309)(NU_IJES(IMA,IM,IMS),IMS=1,4)
          END DO
        END DO  
        
        CLOSE(143)
        
        END DO
        END DO
        END IF      
        
         
        END IF
      
      
      
      
      

  
 
 209  FORMAT(2F18.2,65F14.5,6I10)   
 500  FORMAT(F18.2,70(I10,36F16.5))   
 600  FORMAT(F18.2,70(I10,69F16.5))   
 309  FORMAT(4I8)   
 409  FORMAT(A558)   
      
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

