      PROGRAM main
 
C	USE DFLIB   !传入参数
c      include 'mpif.h'
C
C*********************************************************
CW============SWAT-RCA-ECOM模型结果统计==================
CW===============王永桂于武大2016年======================
CW********************修改记录***************************
CW--------------11月10日，添加SWAT中hru逐年统计----------
CW--------------12月05日，添加RCA结果的3维剖面显示-------
CW--------------12月21日，添加SWAT每个sub的逐年逐月统计-------


CW===============王永桂于2017年修订======================
CW********************修改记录***************************
CW--------------3月14日，添加SWAT每个乡镇的统计--------------
CW--------------4月01日，按照SWAT校核修改--------------------
CW--------------4月05日，添加ECOM结果的断面流量----------
CW--------------4月18日，添加ECOM结果的断面流量的自动统计----------
CW--------------5月01日，将ECOM结果统计流量的第一条边加上----------
CW--------------5月02日，开始使用git进行编辑，并上传----------

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
 
      
 !     ---------此处fname,PATHID中有赋值计算，因此必须放在所有声明的后面---------

      INCLUDE 'INC\PATHID.INC'        
      
      
      USERPATH="D:\GRID_MAKE_DATA\PXH_C2016"   !!!修改路径

      ISECOM=2  !!1进行ecom的统计；2进行rca结果的统计; 3进行swat统计；4进行漂浮物统计；
      
      ISMONITJ=1  ! 1，有监控点的统计；0无监控点
       
      ISQCHECK=1   !!!ecom中进行Q的检验，要求ISECOM=3
      ISQCHEAVE=1   !!!ecom中进行Q的检验，自动统计检验
      
      ISTOALL=0  !!!是否将分块结果变成一块  
      
      ISHRUALL=0   !!!是否输出每个HRU每年的结果     
      
      ISSUBALL=0   !!!是否输出每个SUB每年的结果     
       
      ISRCHALL=0   !!!是否输出每个RCH每年的结果     
      
      
      ISHRUCITY=0    !!!是否进行每个HRU和城镇的统计
      
      ISRCA3D=1    !!!是否要进行三维的统计
      
      ISECOMUV=0    !!!是否要进行流场的展示
      ISDITU=0    !!!是否要加底图地形，2为真三维底图
      
      
      MASTART=105  !!!要汇总的起始块
      MAEND=122    !!!要汇总的结束块
      
      
      NUMHRUS=360   !!!已知的HRU个数    
      
      




      ISSUB=0
      ISHRU=1
      ISRCH=0

      NUMHRU=0
      NUMSUB=0
      
       

      
      CALL INIFNAME
      
      
      IF(ISMONITJ.EQ.1)THEN
      
      
      IF(ISRCA3D.EQ.0)THEN     
       IF(ISECOM.EQ.1)THEN 
        OPEN (14,FILE=FNAME(4),ACTION='READ')     !!!ECOM统计 
       ELSE IF(ISECOM.EQ.2)THEN 
          
        OPEN (14,FILE=FNAME(1),ACTION='READ')     !!!RCA统计，不进行三维输出
      
       ELSE IF(ISECOM.EQ.3)THEN                   
        OPEN (14,FILE=FNAME(2),ACTION='READ')     !!!SWAT统计
      END IF
      
         IF(ISQCHECK.EQ.1)THEN   !!!进行断面的流量统计
              
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



         IF(ISECOM.EQ.1)THEN    !!!ECOM中的统计
             
          
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
           WRITE(*,*)"输出：",trim(FNAME(5))//"_RCH_"//trim(CMA)//".DAT"
           
           OPEN(IMONI(i)+5000,FILE=FNAMMONI(1))
           WRITE(IMONI(i)+5000,6000)"TIME,Q,
     &COD_C,COD_Q,NH4_C,NH4_Q,
     &TN_C,TN_Q,TP_C,TP_Q,TEMP
     &"
         
          ELSE IF(NUMjS(i).EQ.2)THEN
           FNAMMONI(1)=trim(FNAME(5))//"_HRU_"//trim(CMA)//".DAT"
           WRITE(*,*)"输出：",trim(FNAME(5))//"_HRU_"//trim(CMA)//".DAT" 
           
           OPEN(IMONI(i)+20,FILE=FNAMMONI(1))
           WRITE(IMONI(i)+20,6000)"hruID,iida,
     &AREA,PCP,SURQ,ET,TLOSS,TLATQ,GWQ,WYLD,TN_Q,TP_Q,TN_ARE,TP_ARE
     &"          
            
           ELSE IF(NUMjS(i).EQ.3)THEN
            FNAMMONI(1)=trim(FNAME(5))//"_SUB_"//trim(CMA)//".DAT"
         WRITE(*,*)"输出：",trim(FNAME(5))//"_SUB_"//trim(CMA)//".DAT"
           
            OPEN(IMONI(i)+200,FILE=FNAMMONI(1))
           WRITE(IMONI(i)+200,6000)"SUBID,iida,
     &AREA,PRECIP,SLOW,PET,ET,SURQ,GWQ,WYLD,TN_Q,TP_Q,TN_ARE,TP_ARE
     &"      
              
           END IF
       END IF
           
            
         
          do in=1,NUMIS(i)
            read(14,*)IJMONI(i,in)
             
           IF(IJMONI(i,in).GT.IJM)THEN
            WRITE(*,*)IJMONI(i,in),"大于网格总数，请用分块后的网格号！"
            PAUSE
          END IF
          end do
       
       end do
        close(14)
        
        
         IF(ISHRUALL.EQ.1)THEN   !!!每个hru每年的结果值
           FNAMMONI(1)=trim(FNAME(5))//"_HRU_ALL.DAT"
           WRITE(*,*)"输出：",trim(FNAME(5))//"_HRU_ALL.DAT" 
           
           OPEN(220,FILE=FNAMMONI(1))
           WRITE(220,6000)"hruID,IY,
     &AREA,PCP,SURQ,ET,TLOSS,TLATQ,GWQ,WYLD,TN_Q,TP_Q
     &,COD,NH4,SS,TN_ARE,TP_ARE"     
         END IF  
         
     
        IF(ISSUBALL.EQ.1)THEN   
            
CW==============每个SUB每年的结果值================
           FNAMMONI(1)=trim(FNAME(5))//"_SUB_ALL.DAT"
           WRITE(*,*)"输出：",trim(FNAME(5))//"_SUB_ALL.DAT" 
           
           OPEN(220,FILE=FNAMMONI(1))
           WRITE(220,6000)"SUBID,IY,
     &AREA,PCP,SURQ,TN_Q,TP_Q,ET,
     &,COD,NH4,SS,TN_ARE,TP_ARE"     
           
           
           
CW===============逐月统计============
         DO IM=1,12
           write(CMA,'(I2.2)')IM
             
           FNAMMONI(IM)=trim(FNAME(5))//"_SUB_ALL_"//trim(CMA)//".DAT"
          
           OPEN(220+IM,FILE=FNAMMONI(IM))
           WRITE(220+IM,6000)"SUBID,IY,
     &AREA,PCP,SURQ,TN_Q,TP_Q,ET,
     &,COD,NH4,SS,TN_ARE,TP_ARE"         
          
         END DO
           
           
       END IF           
         
         
      IF(ISRCHALL.EQ.1)THEN   !!!每个RCH每年的结果值
           FNAMMONI(1)=trim(FNAME(5))//"_RCH_ALL.DAT"
           WRITE(*,*)"输出：",trim(FNAME(5))//"_RCH_ALL.DAT" 
           
           OPEN(220,FILE=FNAMMONI(1))
           WRITE(220,6000)"RCHID,IY,IM,ID,
     &AREA,PCP,COD_C,COD_Q,NH4_C,NH4_Q,
     &TN_C,TN_Q,TP_C,TP_Q,TEMP"     
       END IF                    
        
      IF(NUMHRU.GT.0)THEN
    
            FNAMMONI(1)=trim(FNAME(5))//"_AVER_HRU"//".DAT"
        WRITE(*,*)"输出：",trim(FNAME(5))//"_AVER_HRU"//".DAT"
           
            OPEN(105,FILE=FNAMMONI(1))
           WRITE(105,*)"TIME,TN_HRU,TP_HRU
     &"       
      END IF
      
       IF(NUMSUB.GT.0)THEN
            FNAMMONI(1)=trim(FNAME(5))//"_AVER_SUB"//".DAT"
       WRITE(*,*)"输出：",trim(FNAME(5))//"_AVER_SUB"//".DAT"
           
         OPEN(106,FILE=FNAMMONI(1))
           WRITE(106,*)"TIME,TN_SUB,TP_SUB
     &"       
       END IF
  
      IF(ISECOM.NE.3)THEN           
       call readtecplot_debug(ISECOM)  !!!读取ecom/rca的tecplot结果
        
      ELSE
       call readoutput(ISECOM)         !!!读取swat的output结果    
       
      END IF
      
      
CW===============三维输出显示======================
   
      ELSE !!!ECOM\RCA三维输出 
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
               
         
       IF(ISHRUCITY.EQ.1)THEN   !!!每个hru对应的城市的值
           FNAMMONI(1)=trim(FNAME(5))//"_HRU_CITY.DAT"
           WRITE(*,*)"输出：",trim(FNAME(5))//"_HRU_CITY.DAT" 
           
           OPEN(240,FILE=FNAMMONI(1))
           WRITE(240,6000)"CITYID,IY,
     &AREA,PCP,SURQ,ET,TLOSS,TLATQ,GWQ,WYLD,TN_Q,TP_Q
     &,COD,NH4,SS,TN_ARE,TP_ARE"     
           
            call readoutput(ISECOM)         !!!读取swat的output结果    
       END IF  
         
       
   
        
        
        
          
      END IF
      
      
      
      
      
       
       WRITE(*,*)"生成完成……"
        
5000   format(a50)
6000   format(a80)
 
        
      END
