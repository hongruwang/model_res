      PROGRAM main
 
C	USE DFLIB   !�������
c      include 'mpif.h'
C
C*********************************************************
CW============SWAT-RCA-ECOMģ�ͽ��ͳ��==================
CW===============�����������2016��======================
CW********************�޸ļ�¼***************************
CW--------------11��10�գ����SWAT��hru����ͳ��----------
CW--------------12��05�գ����RCA�����3ά������ʾ-------
CW--------------12��21�գ����SWATÿ��sub����������ͳ��-------


CW===============��������2017���޶�======================
CW********************�޸ļ�¼***************************
CW--------------3��14�գ����SWATÿ�������ͳ��--------------
CW--------------4��01�գ�����SWATУ���޸�--------------------
CW--------------4��05�գ����ECOM����Ķ�������----------
CW--------------4��18�գ����ECOM����Ķ����������Զ�ͳ��----------
CW--------------5��01�գ���ECOM���ͳ�������ĵ�һ���߼���----------
CW--------------5��02�գ���ʼʹ��git���б༭�����ϴ�----------

C*********************************************************
C
      INCLUDE 'INC\comdeck.INC'
      INCLUDE 'INC\INDEX.INC'
      INCLUDE 'INC\ARCHIVE.INC'
      INCLUDE 'INC\BCDATA.INC'
      INCLUDE 'INC\BCOND.INC'
      INCLUDE 'INC\CALCDAY.INC'
      INCLUDE 'INC\CMA.INC'
      INCLUDE 'INC\CDAY.INC'
      INCLUDE 'INC\DH.INC'
      INCLUDE 'INC\DISPLY.INC'
      INCLUDE 'INC\ECOM3D.INC'
      INCLUDE 'INC\FHCALC.INC'
      INCLUDE 'INC\FLUX.INC'
      INCLUDE 'INC\FMIN.INC'
      INCLUDE 'INC\HITFLX.INC'
      INCLUDE 'INC\PARA.INC'
      INCLUDE 'INC\PROFE.INC'
      INCLUDE 'INC\PROFQ.INC'
      INCLUDE 'INC\PROFSED.INC'
      INCLUDE 'INC\PROFXY.INC'
      INCLUDE 'INC\READDATA.INC'
      INCLUDE 'INC\SEDIC.INC'
      INCLUDE 'INC\SUSLOD.INC'
      INCLUDE 'INC\TANDS.INC'
      INCLUDE 'INC\TRANINP.INC'
      INCLUDE 'INC\TRANSPORT.INC'      
      INCLUDE 'INC\WREAL.INC' 
      INCLUDE 'INC\WVULP.INC'   
      INCLUDE 'INC\ZM.INC'         
      INCLUDE 'INC\floatdebr.INC'
        integer*4 today(3), now(3)
        
        
       NUMMONS(1)=31 
       NUMMONS(2)=NUMMONS(1)+29 
       NUMMONS(3)=NUMMONS(2)+31 
       NUMMONS(4)=NUMMONS(3)+30 
       NUMMONS(5)=NUMMONS(4)+31 
       NUMMONS(6)=NUMMONS(5)+30 
       NUMMONS(7)=NUMMONS(6)+31 
       NUMMONS(8)=NUMMONS(7)+31 
       NUMMONS(9)=NUMMONS(8)+30 
       NUMMONS(10)=NUMMONS(9)+31 
       NUMMONS(11)=NUMMONS(10)+30 
       NUMMONS(12)=NUMMONS(11)+31 
 
      
 !     ---------�˴�fname,PATHID���и�ֵ���㣬��˱���������������ĺ���---------

      INCLUDE 'INC\PATHID.INC'        
      
      
      USERPATH="D:\GRID_MAKE_DATA\PXH_C2016"   !!!�޸�·��

      ISECOM=2  !!1����ecom��ͳ�ƣ�2����rca�����ͳ��; 3����swatͳ�ƣ�4����Ư����ͳ�ƣ�
      
      ISMONITJ=1  ! 1���м�ص��ͳ�ƣ�0�޼�ص�
       
      ISQCHECK=1   !!!ecom�н���Q�ļ��飬Ҫ��ISECOM=3
      ISQCHEAVE=1   !!!ecom�н���Q�ļ��飬�Զ�ͳ�Ƽ���
      
      ISTOALL=0  !!!�Ƿ񽫷ֿ������һ��  
      
      ISHRUALL=0   !!!�Ƿ����ÿ��HRUÿ��Ľ��     
      
      ISSUBALL=0   !!!�Ƿ����ÿ��SUBÿ��Ľ��     
       
      ISRCHALL=0   !!!�Ƿ����ÿ��RCHÿ��Ľ��     
      
      
      ISHRUCITY=0    !!!�Ƿ����ÿ��HRU�ͳ����ͳ��
      
      ISRCA3D=1    !!!�Ƿ�Ҫ������ά��ͳ��
      
      ISECOMUV=0    !!!�Ƿ�Ҫ����������չʾ
      ISDITU=0    !!!�Ƿ�Ҫ�ӵ�ͼ���Σ�2Ϊ����ά��ͼ
      
      
      MASTART=105  !!!Ҫ���ܵ���ʼ��
      MAEND=122    !!!Ҫ���ܵĽ�����
      
      
      NUMHRUS=360   !!!��֪��HRU����    
      
      




      ISSUB=0
      ISHRU=1
      ISRCH=0

      NUMHRU=0
      NUMSUB=0
      
       

      
      CALL INIFNAME
      
      
      IF(ISMONITJ.EQ.1)THEN
      
      
      IF(ISRCA3D.EQ.0)THEN     
       IF(ISECOM.EQ.1)THEN 
        OPEN (14,FILE=FNAME(4),ACTION='READ')     !!!ECOMͳ�� 
       ELSE IF(ISECOM.EQ.2)THEN 
          
        OPEN (14,FILE=FNAME(1),ACTION='READ')     !!!RCAͳ�ƣ���������ά���
      
       ELSE IF(ISECOM.EQ.3)THEN                   
        OPEN (14,FILE=FNAME(2),ACTION='READ')     !!!SWATͳ��
      END IF
      
         IF(ISQCHECK.EQ.1)THEN   !!!���ж��������ͳ��
              
            FNAMMONI(2)=trim(FNAME(9))//"_QCHECK.DAT"
            
          OPEN(22000,FILE=FNAMMONI(2))
          WRITE(22000,*)"TIME,I,MA,IJ1,Q"
        
          FNAMMONI(2)=trim(FNAME(9))//"_QCHECK_ALL.DAT"
          
         OPEN(22100,FILE=FNAMMONI(2))
          WRITE(221000,*)"TIME,I,MA,IJ1,Q"
       
         
           FNAMMONI(2)=trim(FNAME(9))//"_QCHECK_4B.DAT"
          
         OPEN(222000,FILE=FNAMMONI(2))
          WRITE(222000,*)"TIME,IJ,MA,H1P(IJ),H2P(IJ),(
     &U(IJ,k),
     &V(IJ,k),
     &W(IJ,K),K=1,KB-1),EL(IJ)  "
202       Format(A50)         
          END IF    
      
       read(14,*) numtime1,numtime2,MASTART,MAEND
        
       read(14,*) mmoni
       do i=1,mmoni
          read(14,*)IMONI(i),NUMIS(i),NUMjS(i)
          IF(NUMjS(i).EQ.1)THEN
            ISRCH=1
          ELSE IF(NUMjS(i).EQ.2)THEN
            ISHRU=1  
            NUMHRU=NUMHRU+1
            I2ID(I)=NUMHRU
          ELSE IF(NUMjS(i).EQ.3)THEN
            ISSUB=1  
            NUMSUB=NUMSUB+1   
            I3ID(I)=NUMSUB            
          END IF
           
           IF(IMONI(i).LE.99999)THEN
            write(CMA,'(I5.4)')IMONI(i)
           ELSE
            write(CMA,'(I10.4)')IMONI(i)          
           END IF



         IF(ISECOM.EQ.1)THEN    !!!ECOM�е�ͳ��
             
          
          FNAMMONI(1)=trim(FNAME(9))//"_IJ_"//trim(CMA)//".DAT"
          OPEN(IMONI(i)+5000,FILE=FNAMMONI(1))
          WRITE(IMONI(i)+5000,5000)"TIME,IJ,X,
     &Y,ZB ,EL,
     &WU,WV,
     &U1,V1,
     &W1,T1,Q21,
     &L1 "
          
          
             
          
          
          
          ELSE IF(ISECOM.EQ.2)THEN 

         
        FNAMMONI(1)=trim(FNAME(8))//"_IJ_"//trim(CMA)//".DAT"
          OPEN(IMONI(i)+5000,FILE=FNAMMONI(1))
          WRITE(IMONI(i)+5000,6000)"TIME,IJ,X,
     &Y,ZB ,EL,
     &TN1,TP1,
     &COD1,NH41,CHA1,
     &T1,PC1,PC2,PC3,DO,
     &LIGHT1,GRW1,LOSS1,
     &LIGHT1,GRW1,LOSS1,
     &LIGHT1,GRW1,LOSS1 "
          
          
        ELSE IF(ISECOM.EQ.3)THEN 
              

         IF(NUMjS(i).EQ.1)THEN
           FNAMMONI(1)=trim(FNAME(5))//"_RCH_"//trim(CMA)//".DAT"
           WRITE(*,*)"�����",trim(FNAME(5))//"_RCH_"//trim(CMA)//".DAT"
           
           OPEN(IMONI(i)+5000,FILE=FNAMMONI(1))
           WRITE(IMONI(i)+5000,6000)"TIME,Q,
     &COD_C,COD_Q,NH4_C,NH4_Q,
     &TN_C,TN_Q,TP_C,TP_Q,TEMP
     &"
         
          ELSE IF(NUMjS(i).EQ.2)THEN
           FNAMMONI(1)=trim(FNAME(5))//"_HRU_"//trim(CMA)//".DAT"
           WRITE(*,*)"�����",trim(FNAME(5))//"_HRU_"//trim(CMA)//".DAT" 
           
           OPEN(IMONI(i)+20,FILE=FNAMMONI(1))
           WRITE(IMONI(i)+20,6000)"hruID,iida,
     &AREA,PCP,SURQ,ET,TLOSS,TLATQ,GWQ,WYLD,TN_Q,TP_Q,TN_ARE,TP_ARE
     &"          
            
           ELSE IF(NUMjS(i).EQ.3)THEN
            FNAMMONI(1)=trim(FNAME(5))//"_SUB_"//trim(CMA)//".DAT"
         WRITE(*,*)"�����",trim(FNAME(5))//"_SUB_"//trim(CMA)//".DAT"
           
            OPEN(IMONI(i)+200,FILE=FNAMMONI(1))
           WRITE(IMONI(i)+200,6000)"SUBID,iida,
     &AREA,PRECIP,SLOW,PET,ET,SURQ,GWQ,WYLD,TN_Q,TP_Q,TN_ARE,TP_ARE
     &"      
              
           END IF
       END IF
           
            
         
          do in=1,NUMIS(i)
            read(14,*)IJMONI(i,in)
             
           IF(IJMONI(i,in).GT.IJM)THEN
            WRITE(*,*)IJMONI(i,in),"�����������������÷ֿ�������ţ�"
            PAUSE
          END IF
          end do
       
       end do
        close(14)
        
        
         IF(ISHRUALL.EQ.1)THEN   !!!ÿ��hruÿ��Ľ��ֵ
           FNAMMONI(1)=trim(FNAME(5))//"_HRU_ALL.DAT"
           WRITE(*,*)"�����",trim(FNAME(5))//"_HRU_ALL.DAT" 
           
           OPEN(220,FILE=FNAMMONI(1))
           WRITE(220,6000)"hruID,IY,
     &AREA,PCP,SURQ,ET,TLOSS,TLATQ,GWQ,WYLD,TN_Q,TP_Q
     &,COD,NH4,SS,TN_ARE,TP_ARE"     
         END IF  
         
     
        IF(ISSUBALL.EQ.1)THEN   
            
CW==============ÿ��SUBÿ��Ľ��ֵ================
           FNAMMONI(1)=trim(FNAME(5))//"_SUB_ALL.DAT"
           WRITE(*,*)"�����",trim(FNAME(5))//"_SUB_ALL.DAT" 
           
           OPEN(220,FILE=FNAMMONI(1))
           WRITE(220,6000)"SUBID,IY,
     &AREA,PCP,SURQ,TN_Q,TP_Q,ET,
     &,COD,NH4,SS,TN_ARE,TP_ARE"     
           
           
           
CW===============����ͳ��============
         DO IM=1,12
           write(CMA,'(I2.2)')IM
             
           FNAMMONI(IM)=trim(FNAME(5))//"_SUB_ALL_"//trim(CMA)//".DAT"
          
           OPEN(220+IM,FILE=FNAMMONI(IM))
           WRITE(220+IM,6000)"SUBID,IY,
     &AREA,PCP,SURQ,TN_Q,TP_Q,ET,
     &,COD,NH4,SS,TN_ARE,TP_ARE"         
          
         END DO
           
           
       END IF           
         
         
      IF(ISRCHALL.EQ.1)THEN   !!!ÿ��RCHÿ��Ľ��ֵ
           FNAMMONI(1)=trim(FNAME(5))//"_RCH_ALL.DAT"
           WRITE(*,*)"�����",trim(FNAME(5))//"_RCH_ALL.DAT" 
           
           OPEN(220,FILE=FNAMMONI(1))
           WRITE(220,6000)"RCHID,IY,IM,ID,
     &AREA,PCP,COD_C,COD_Q,NH4_C,NH4_Q,
     &TN_C,TN_Q,TP_C,TP_Q,TEMP"     
       END IF                    
        
      IF(NUMHRU.GT.0)THEN
    
            FNAMMONI(1)=trim(FNAME(5))//"_AVER_HRU"//".DAT"
        WRITE(*,*)"�����",trim(FNAME(5))//"_AVER_HRU"//".DAT"
           
            OPEN(105,FILE=FNAMMONI(1))
           WRITE(105,*)"TIME,TN_HRU,TP_HRU
     &"       
      END IF
      
       IF(NUMSUB.GT.0)THEN
            FNAMMONI(1)=trim(FNAME(5))//"_AVER_SUB"//".DAT"
       WRITE(*,*)"�����",trim(FNAME(5))//"_AVER_SUB"//".DAT"
           
         OPEN(106,FILE=FNAMMONI(1))
           WRITE(106,*)"TIME,TN_SUB,TP_SUB
     &"       
       END IF
  
      IF(ISECOM.NE.3)THEN           
       call readtecplot_debug(ISECOM)  !!!��ȡecom/rca��tecplot���
        
      ELSE
       call readoutput(ISECOM)         !!!��ȡswat��output���    
       
      END IF
      
      
CW===============��ά�����ʾ======================
   
      ELSE !!!ECOM\RCA��ά��� 
       IF(ISECOM.EQ.1)THEN     
        OPEN (314,FILE=FNAME(13),ACTION='READ')    
        read(314,*)NUM3DS,numtime1,numtime2 
        do i=1,NUM3DS
          read(314,*)IK,NUM3DK(I),NUM3DKIJ(I)
          DO J=1,NUM3DKIJ(I)
           read(314,*)IK,NUMZIJS(I,J),(IJ3DIJ(I,J,II),II=1,NUMZIJS(I,J))
          END DO
        END DO
        CLOSE(314)
        
        CALL  readtecplot_ECOM_3D(ISDITU)
        
       ELSE
        OPEN (314,FILE=FNAME(6),ACTION='READ')    
        read(314,*)NUM3DS,numtime1,numtime2 
        do i=1,NUM3DS
          read(314,*)IK,NUM3DK(I),NUM3DKIJ(I)
          DO J=1,NUM3DKIJ(I)
           read(314,*)IK,NUMZIJS(I,J),(IJ3DIJ(I,J,II),II=1,NUMZIJS(I,J))
          END DO
        END DO
        CLOSE(314)
        CALL  readtecplot_RCA_3D(ISDITU)
       END IF      
         
      ENDIF
      
      ELSE
               
         
       IF(ISHRUCITY.EQ.1)THEN   !!!ÿ��hru��Ӧ�ĳ��е�ֵ
           FNAMMONI(1)=trim(FNAME(5))//"_HRU_CITY.DAT"
           WRITE(*,*)"�����",trim(FNAME(5))//"_HRU_CITY.DAT" 
           
           OPEN(240,FILE=FNAMMONI(1))
           WRITE(240,6000)"CITYID,IY,
     &AREA,PCP,SURQ,ET,TLOSS,TLATQ,GWQ,WYLD,TN_Q,TP_Q
     &,COD,NH4,SS,TN_ARE,TP_ARE"     
           
            call readoutput(ISECOM)         !!!��ȡswat��output���    
       END IF  
         
       
   
        
        
        
          
      END IF
      
      
      
      
      
       
       WRITE(*,*)"������ɡ���"
        
5000   format(a50)
6000   format(a80)
 
        
      END
