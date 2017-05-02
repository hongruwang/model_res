      SUBROUTINE readoutput(ISRCA)
    
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
CW==========11.10添加hru统计===================      
      
      DIMENSION KE_P(IJM),FVAR(200),FHRUS(3200,2,366,80),hruARE(3200)
      DIMENSION IJ_P(IJM),FSUMHRU(3000,30,80),FHRUT(20,30)
      DIMENSION FSUMSUB(500,300,12,80),FSUBT(12,200,300),NUMCITS(200)
      DIMENSION FSUCITYS(200,3000),IDHURCITY(200,3000)
      DIMENSION VEHURCITY(200,3000)
      CHARACTER*50 VERT,TMTEC,CMA,FSUB,FHRU
      CHARACTER*150  IYTEC,FNAMERES6,FNAMERES7,FNAMERES8
      logical isexit
      INTEGER ISRCA
    
CW    =========输出tecplot可以看得文件==============      
      itotr=46
      itothru=78
      itotsub=22
      IF(mmoni.GT.0)THEN
          IF(ISRCH.EQ.1)THEN  !!!!读取output.rch文件
            OPEN(100,FILE=FNAME(10))  !!!
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT    
           DO IN=1,9999999  
           READ(100,5000,end=100)VERT,ISUB,iimo,iida,       
     &             rch_dakm, (FVAR(ii), ii = 1, itotr)  


           
            IF(iida.Ge.numtime1.AND.iida.le.numtime2)THEN       
             COD=FVAR(23)/(FVAR(2)* 86.4)
             FNH4_C=FVAR(15)/(FVAR(2)* 86.4)
             TN_C=FVAR(43)/(FVAR(2)* 86.4)
             TP_C=FVAR(44)/(FVAR(2)* 86.4)
             TEMP=FVAR(46)
     
             DO i=1,mmoni 
                IF(NUMjS(i).EQ.1)THEN !!!输出监控RCH要的结果
                  IF(ISUB.EQ.IJMONI(i,1))THEN
                  WRITE(IMONI(i)+5000,500)iida,FVAR(2),COD,FVAR(23),
     &FNH4_C,FVAR(15),TN_C,FVAR(43),TP_C,FVAR(44),TEMP
                  END IF
                END IF
             END DO
            END IF
           END DO
           
           
 5000 format (A6,i4,1x,i8,1x,i5,47e12.4)     
 500  format (i5,47e12.4)     
           
       
100       close(100) 
          END IF
          
      IF(ISHRU.EQ.1)THEN  !!!!读取output.hru文件
            OPEN(100,FILE=FNAME(11))  !!!
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT    
            
           iidafor=0  
           

           DO IN=1,9999999  
           iidafor=iida    
           READ(100,1001,end=200)VERT,ihru, Fsub, 
     &      Fhru, Isb, nmgt, iida, hru_km,
     &(FVAR(ii), ii = 1, itothru)  
           
           
1001   format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,e10.5,1x,
     &e10.5,8e10.3,2f10.3)
      
             IF(iida.ge.numtime1.AND.iida.le.numtime2)THEN        
              AREA=hru_km
              PCP=FVAR(1)
              SURQ=FVAR(17)
              ET=FVAR(6)
              TLOSS=FVAR(19)
              TLATQ=FVAR(20)
              GWQ=FVAR(21)
              WYLD=FVAR(22)
             
              TN_Q=FVAR(51)+FVAR(54)+FVAR(55)+FVAR(57)
              TP_Q=FVAR(52)+FVAR(58)+FVAR(59)



              COD=FVAR(23)/(FVAR(2)* 86.4) !!!COD 校核

              NH4=FVAR(57)     !!!氨氮 校核

              TN_ARE=TN_Q/AREA
              TP_ARE=TP_Q/AREA
	        SED=FVAR(29)*AREA*100
              
              IF(IIDAFOR.NE.IIDA)THEN
                TN_AVER=0
                TP_AVER=0
              END IF

         
               DO i=1,mmoni 
                IF(NUMjS(i).EQ.2)THEN !!!输出监控HRU要的结果
                  IF(ihru.EQ.IJMONI(i,1))THEN
                  WRITE(IMONI(i)+2000,600)ihru, iida,
     &AREA,PCP,SURQ,ET,TLOSS,TLATQ,GWQ,WYLD,TN_Q,TP_Q,TN_ARE,
     &TP_ARE,COD,NH4
                  
                   TN_AVER=TN_AVER+TN_ARE
                   TP_AVER=TN_AVER+TP_ARE
600    FORMAT(I10, I10,30F20.8)                       
                   IF(I2ID(I).EQ.NUMHRU)THEN
                   WRITE(105,*)iida,TN_AVER/NUMHRU,TP_AVER/NUMHRU
                   END IF
                  
                  END IF
                END IF
               END DO
            
            END IF
           END DO
  
 200      close(IMONI(i)+5000) 
          close(100) 
          close(105) 
   
         END IF          
         

         
         
       IF(ISSUB.EQ.1)THEN  !!!!读取output.sub文件
             TN_AVER=0
             TP_AVER=0
            OPEN(100,FILE=FNAME(12))  !!!
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT    
  
            iidafor=0  
           

           DO IN=1,9999999  
           iidafor=iida    
           READ(100,1000,end=300)VERT,IDSUB, ISUB, iida, sub_km,
     &       (FVAR(ii), ii = 1, itotsub)
           
           
 1000 format (A6,i4,1x,i8,1x,i4,e10.5,18e10.3,1x,e10.5,3e10.3)
      
             IF(iida.ge.numtime1.AND.iida.le.numtime2)THEN        
              AREA=sub_km
              PRECIP=FVAR(1)
              SLOW=FVAR(2)
              PET=FVAR(3)
              ET=FVAR(4)
              SURQ=FVAR(7)
c	        write(*,*)SURQ,"流量"
              GWQ=FVAR(8)
              WYLD=FVAR(9)
             
              TN_Q=FVAR(11)+FVAR(13) 
              
              TP_Q=FVAR(12)+FVAR(14) 
              
              TN_ARE=TN_Q/AREA
              TP_ARE=TP_Q/AREA
              
             IF(IIDAFOR.NE.IIDA)THEN
                TN_AVER=0
                TP_AVER=0
             END IF
             
             
               DO i=1,mmoni 
                IF(NUMjS(i).EQ.3)THEN !!!输出监控sub要的结果
                  IF(IDSUB.EQ.IJMONI(i,1))THEN
                  WRITE(IMONI(i)+8000,700)IDSUB,iida,
     &AREA,PRECIP,SLOW,PET,ET,SURQ,GWQ,WYLD,TN_Q,TP_Q,TN_ARE,TP_ARE,SED
                  
                   TN_AVER=TN_AVER+TN_ARE
                   TP_AVER=TP_AVER+TP_ARE
                  
                   IF(I3ID(I).EQ.NUMSUB)THEN
C                    WRITE(106,*)iida,TN_AVER/NUMSUB,TP_AVER/NUMSUB
                     WRITE(106,*)i,TN_Q,TP_Q,SED
                   END IF
                  END IF
                END IF
               END DO
            END IF
           END DO
  
 300         close(IMONI(i)+5000) 
          close(100) 
          close(106) 
      
         END IF           
         
         
         
         
      
700    FORMAT(2I10,20F12.4)          
          
      END IF
      
CW**************************************************************
CW*******************每年每个SUB统计****************************
CW**************************************************************

  	IF(ISSUBALL.EQ.1)THEN   !!!输出全部SUB结果

      IF(ISSUB.EQ.1) THEN  !!!!读取output.SUB文件
    
             TN_AVER=0
             TP_AVER=0
            OPEN(100,FILE=FNAME(12))  !!!
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT    
  
           iidafor=0  
           
           NUMDAYS=0
	     IYEAR=1
           

           DO IN=1,9999999  
           iidafor=iida    
           READ(100,1000,end=301)VERT,IDSUB, ISUB, iida, hruARE(IDSUB),
     &       (FHRUS(IDSUB,IYEAR,iida,ii), ii = 1, itotsub)
           
            IF(ihru.EQ.NUMHRUS.AND.iida.EQ.365)THEN   !!!801个SUB
              IYEAR=IYEAR+1
            END IF
           
           END DO                    
          
301       CLOSE(100)
          
CW********************逐月统计******************** 
CW********************统计SUB******************** 
      DO IY=1,IYEAR-1
            DO ISB=1,NUMHRUS    
	       DO ii=1,itothru
              DO ID=1,366
               IF(ID.LE.31)THEN   
       FSUMSUB(ISB,IY,1,II)=FSUMSUB(ISB,IY,1,II)+FHRUS(ISB,IY,ID,II)  
               ELSE
                   
                DO IM=2,12   
                 IF(ID.GT.NUMMONS(IM-1).AND.ID.LE.NUMMONS(IM))THEN   
       FSUMSUB(ISB,IY,IM,II)=FSUMSUB(ISB,IY,IM,II)+FHRUS(ISB,IY,ID,II) 
                 END IF
                END DO 
               END IF
              END DO
             END DO
	      END DO
           END DO

         DO IM=1,12	    
          DO IY=1,IYEAR-1	 
            DO ISB=1,NUMHRUS
 
		    FSUBT(IY,IM,1)=hruARE(ISB)  !!!面积
              
              FSUBT(IY,IM,2)=FSUMSUB(ISB,IY,IM,1)!!!  pcp

              FSUBT(IY,IM,3)=FSUMSUB(ISB,IY,IM,7)*
     &hruARE(ISB)*1000/86400   !!! surfq
 
              FSUBT(IY,IM,4)=FSUMSUB(ISB,IY,IM,4) !!!  et
             
              FSUBT(IY,IM,5)=
     &(FSUMSUB(ISB,IY,IM,11)+FSUMSUB(ISB,IY,IM,13))*hruARE(ISB)*100 !!TN

               FSUBT(IY,IM,6)=
     &(FSUMSUB(ISB,IY,IM,12)+FSUMSUB(ISB,IY,IM,14))*hruARE(ISB)*100 !!TP
               
            
              !!!COD=COD+FVAR(23)/(FVAR(2)* 86.4) !!!COD 校核 
              FSUBT(IY,IM,7)=(FSUMSUB(ISB,IY,IM,51)/
     &(FSUMSUB(ISB,IY,IM,54)*86.4))*hruARE(ISB)*100   

              !!NH4=NH4+FVAR(57)     !!!氨氮 校核
              FSUBT(IY,IM,8)=FSUMSUB(ISB,IY,IM,57)
     &*hruARE(ISB)*100
              
              
              !!SS                    !!!泥沙 校核
              FSUBT(IY,IM,9)=FSUMSUB(ISB,IY,IM,8)
     **hruARE(ISB)*100 
              
              FSUBT(IY,IM,10)=
     &FSUMSUB(ISB,IY,IM,11)+FSUMSUB(ISB,IY,IM,13) !!TN_AREA

              FSUBT(IY,IM,11)=
     &FSUMSUB(ISB,IY,IM,12)+FSUMSUB(ISB,IY,IM,14) !!TP_AREA   

              WRITE(220+IM,620)ISB,IY,IM,(FSUBT(IY,IM,IS),IS=1,11)
620      FORMAT(3I10,30F20.5)
            END DO
          END DO
      END DO
      close(220+IM)          
          
          
CW**************************************************************
CW*******************每年每个HRU统计****************************
CW**************************************************************    
CW==============每年统计==============           
           DO IY=1,IYEAR-1
             DO ISB=1,NUMHRUS    
	       DO ii=1,itothru
              DO ID=1,366
               FSUMHRU(ISB,IY,II)=FSUMHRU(ISB,IY,II)+FHRUS(ISB,IY,ID,II)
              END DO
             END DO
	      END DO
           END DO

           
        
       DO IY=1,IYEAR-1	 
         DO ISB=1,NUMHRUS   
!!!              AREA=hruARE(ISB)   !!!面积
!!!              PRECIP=FVAR(1)     !!!降雨
!!!              SLOW=FVAR(2)
!!!              PET=FVAR(3)
!!!              ET=FVAR(4)         !!!蒸发
!!!              SURQ=FVAR(7)
!!!c	        write(*,*)SURQ,"流量"
!!!              GWQ=FVAR(8)        !!!产流
!!!              WYLD=FVAR(9)
!!!	       
!!!              SED=SED+FVAR(10)*AREA*100    !!!含沙量
!!!              TN_Q=TN_Q+(FVAR(11)+FVAR(13))*AREA*100  !!!TN
!!!              
!!!              TP_Q=TP_Q+(FVAR(12)+FVAR(14))*AREA*100  !!!TP
!!!              
!!!              TN_ARE=TN_Q
!!!              TP_ARE=TP_Q
!!!              
!!!             IF(IIDAFOR.NE.IIDA)THEN
!!!                TN_AVER=0
!!!                TP_AVER=0
!!!             END IF
!!!      AREA,PCP,SURQ,ET,TN_Q,TP_Q
!!!      COD,NH4,SS,TN_ARE,TP_ARE"                          
		    FHRUT(IY,1)=hruARE(IHR)
              
              FHRUT(IY,2)=FHRUT(IY,3)+FSUMHRU(IHR,IY,1)!!!  pcp

              FHRUT(IY,3)=FSUMHRU(IHR,IY,17)*
     &hruARE(IHR)*1000/86400   !!! surfq
 
              FHRUT(IY,4)=FHRUT(IY,4)+FSUMHRU(IHR,IY,6) !!!  et
             
              FHRUT(IY,5)=FHRUT(IY,5)+FSUMHRU(IHR,IY,19) !!TLOSS=TLOSS+FVAR(19)

              FHRUT(IY,6)=FHRUT(IY,6)+FSUMHRU(IHR,IY,20) !!TLATQ=TLATQ+FVAR(20)
              FHRUT(IY,7)=FHRUT(IY,7)+FSUMHRU(IHR,IY,21)  !!GWQ=GWQ+FVAR(21)
              FHRUT(IY,8)=FHRUT(IY,8)+FSUMHRU(IHR,IY,22) !!WYLD=WYLD+FVAR(22)
             
          	!!TN_Q=TN_Q+FVAR(51)+FVAR(54)+FVAR(55)+FVAR(57)
c              FHRUT(IY,9)=FHRUT(IY,9)+FSUMHRU(IHR,IY,54)

c               FHRUT(IY,9)=(FSUMHRU(IHR,IY,54) 
c     &+FSUMHRU(IHR,IY,55) +FSUMHRU(IHR,IY,57) +FSUMHRU(IHR,IY,51))
c    &*100000/FSUMHRU(IHR,IY,17)   !计算浓度TN
            FHRUT(IY,9)=(FSUMHRU(IHR,IY,54) 
     &+FSUMHRU(IHR,IY,55) +FSUMHRU(IHR,IY,57) +FSUMHRU(IHR,IY,51))*
     &hruARE(IHR)*100   !计算负荷(kg)

	        !!TP_Q=TP_Q+FVAR(52)+FVAR(58)+FVAR(59)
c              FHRUT(IY,10)=FHRUT(IY,10)+FSUMHRU(IHR,IY,52)

c			FHRUT(IY,10)=(FSUMHRU(IHR,IY,52) 
c     &+FSUMHRU(IHR,IY,58) +FSUMHRU(IHR,IY,59))                    
c     &*100000/FSUMHRU(IHR,IY,17)   !计算浓度TP
			FHRUT(IY,10)=(FSUMHRU(IHR,IY,52)    
     &+FSUMHRU(IHR,IY,58) +FSUMHRU(IHR,IY,59))*                    
     &hruARE(IHR)*100   !计算负荷(kg)


              !!!COD=COD+FVAR(23)/(FVAR(2)* 86.4) !!!COD 校核 
              FHRUT(IY,11)=FSUMHRU(IHR,IY,51)/
     &(FSUMHRU(IHR,IY,54)*86.4)   

              !!NH4=NH4+FVAR(57)     !!!硝氮 校核
              FHRUT(IY,12)=FSUMHRU(IHR,IY,57)*
     &hruARE(IHR)*100  

	                                   !泥沙  增加
              FHRUT(IY,13)=FSUMHRU(IHR,IY,29)*
     &hruARE(IHR)*100 


              !!!!TN_ARE=TN_ARE+TN_Q/AREA  !不能用
c              FHRUT(IY,13)=FHRUT(IY,13)+FHRUT(IY,9)/hruARE(IHR)*0.01 
               FHRUT(IY,14)=FHRUT(IY,9)/hruARE(IHR)*0.01 


	        !!TP_ARE=TP_ARE+TP_Q/AREA   !不能用
c              FHRUT(IY,14)=FHRUT(IY,15)+FHRUT(IY,10)/hruARE(IHR)*0.01
               FHRUT(IY,15)=FHRUT(IY,10)/hruARE(IHR)*0.01
               
               
           WRITE(220,600)ISB,IY,(FHRUT(IY,is),is=1,11)
 
           END DO
         END DO
 
          close(220)           
    
         END IF
      END IF      
      
       
      
CW**************************************************************
CW*******************每年每个HRU统计****************************
CW**************************************************************
   	IF(ISHRUALL.EQ.1)THEN   !!!输出全部HRU结果

      IF(ISHRU.EQ.1) THEN  !!!!读取output.hru文件
            OPEN(100,FILE=FNAME(11))   
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT    
            
           iidafor=0  
           
           NUMDAYS=0
	     IYEAR=1
           DO IN=1,9999999  
           READ(100,1001,end=210)VERT,ihru, Fsub, 
     &      Fhru, Isb, nmgt, iida, hruARE(IHRU),
     &(FHRUS(IHRU,IYEAR,iida,ii), ii = 1, itothru)  
       

	      IF(ihru.EQ.NUMHRUS.AND.iida.EQ.365)THEN   !!!801个HRU
              IYEAR=IYEAR+1
	      END IF
	        
	     END DO
 
210      CLOSE(100)
          FSUMHRU=0
           DO IY=1,IYEAR-1
            DO IHR=1,NUMHRUS
	       DO ii=1,itothru
              DO ID=1,366
               FSUMHRU(IHR,IY,II)=FSUMHRU(IHR,IY,II)+FHRUS(IHR,IY,ID,II)
              END DO
             END DO
	      END DO
           END DO


          DO IY=1,IYEAR-1	 

            DO IHR=1,NUMHRUS
		    FHRUT(IY,1)=hruARE(IHR)
              
              FHRUT(IY,2)= FSUMHRU(IHR,IY,1)!!!  pcp

              FHRUT(IY,3)= FSUMHRU(IHR,IY,17)*
     &hruARE(IHR)*1000/86400   !!! surfq
 
              FHRUT(IY,4)= FSUMHRU(IHR,IY,6) !!!  et
             
              FHRUT(IY,5)= FSUMHRU(IHR,IY,19) !!TLOSS=TLOSS+FVAR(19)

              FHRUT(IY,6)= FSUMHRU(IHR,IY,20) !!TLATQ=TLATQ+FVAR(20)
              FHRUT(IY,7)= FSUMHRU(IHR,IY,21)  !!GWQ=GWQ+FVAR(21)
              FHRUT(IY,8)= FSUMHRU(IHR,IY,22) !!WYLD=WYLD+FVAR(22)
             
          	!!TN_Q=TN_Q+FVAR(51)+FVAR(54)+FVAR(55)+FVAR(57)
c              FHRUT(IY,9)=FHRUT(IY,9)+FSUMHRU(IHR,IY,54)

c             FHRUT(IY,9)=(FSUMHRU(IHR,IY,54) 
c     &+FSUMHRU(IHR,IY,55) +FSUMHRU(IHR,IY,57) +FSUMHRU(IHR,IY,51))
c    &*100000/FSUMHRU(IHR,IY,17)   !计算浓度TN
            FHRUT(IY,9)=(FSUMHRU(IHR,IY,54) 
     &+FSUMHRU(IHR,IY,55) +FSUMHRU(IHR,IY,57) +FSUMHRU(IHR,IY,51))*
     &hruARE(IHR)*100   !计算负荷(kg)

	        !!TP_Q=TP_Q+FVAR(52)+FVAR(58)+FVAR(59)
c              FHRUT(IY,10)=FHRUT(IY,10)+FSUMHRU(IHR,IY,52)

c			FHRUT(IY,10)=(FSUMHRU(IHR,IY,52) 
c     &+FSUMHRU(IHR,IY,58) +FSUMHRU(IHR,IY,59))                    
c     &*100000/FSUMHRU(IHR,IY,17)   !计算浓度TP
			FHRUT(IY,10)=(FSUMHRU(IHR,IY,52)    
     &+FSUMHRU(IHR,IY,58) +FSUMHRU(IHR,IY,59))*                    
     &hruARE(IHR)*100   !计算负荷(kg)


              !!!COD=COD+FVAR(23)/(FVAR(2)* 86.4) !!!COD 校核 
              FHRUT(IY,11)= FSUMHRU(IHR,IY,51)/
     &(FSUMHRU(IHR,IY,54)*86.4)   

              !!NH4=NH4+FVAR(57)     !!!氨氮 校核
              FHRUT(IY,16)= FSUMHRU(IHR,IY,57) 

                                      !泥沙  增加
              FHRUT(IY,13)=FSUMHRU(IHR,IY,29)*
     &hruARE(IHR)*100 


	        !!TP_ARE=TP_ARE+TP_Q/AREA
c              FHRUT(IY,14)=FHRUT(IY,15)+FHRUT(IY,10)/hruARE(IHR)*0.01
               FHRUT(IY,14)=FHRUT(IY,9)/hruARE(IHR)*0.01
               FHRUT(IY,15)=FHRUT(IY,10)/hruARE(IHR)*0.01
               
               
              WRITE(220,600)IHR,IY,(FHRUT(IY,is),is=1,15)

	       END DO
            END DO

         
          close(220) 
         END IF
      END IF
 
      
        
      
CW**************************************************************
CW*******************每年每个城市对应的HRU统计******************
CW**************************************************************
   	IF(ISHRUCITY.EQ.1)THEN   !!!输出全部HRU结果
          
        
      IF(ISHRU.EQ.1) THEN  !!!!读取output.hru文件
            OPEN(100,FILE=FNAME(11))   
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT
             READ(100,*)VERT    
            
           iidafor=0  
           
           NUMDAYS=0
	     IYEAR=1
           DO IN=1,9999999  
           READ(100,1001,end=2110)VERT,ihru, Fsub, 
     &      Fhru, Isb, nmgt, iida, hruARE(IHRU),
     &(FHRUS(IHRU,IYEAR,iida,ii), ii = 1, itothru)  
       

	      IF(ihru.EQ.NUMHRUS.AND.iida.EQ.365)THEN   !!!801个HRU
              IYEAR=IYEAR+1
	      END IF
	        
	     END DO
 
2110      CLOSE(100)
          FSUMHRU=0
           DO IY=1,IYEAR-1
            DO IHR=1,NUMHRUS
	       DO ii=1,itothru
              DO ID=1,366
               FSUMHRU(IHR,IY,II)=FSUMHRU(IHR,IY,II)+FHRUS(IHR,IY,ID,II)
              END DO
             END DO
	      END DO
           END DO

           
         OPEN(408,FILE=FNAME(48))
         
         READ(408,*)NUMCITY
            write(*,*)NUMCITY
       DO INN=1,NUMCITY  
         READ(408,*)IK,NUMCITS(INN)
         write(*,*)IK,NUMCITS(INN)
          DO IDD=1,NUMCITS(INN)
           READ(408,*) IDHURCITY(INN,IDD),AREATEMP
     
           VEHURCITY(INN,IDD)=AREATEMP/hruARE(IDHURCITY(INN,IDD))
       
          END DO
       END DO
       
       
       
       close(408)
       
       
       
       
       
       DO IY=1,IYEAR-1
        FSUCITYS=0   
        DO INN=1,NUMCITY  
         DO IDD=1,NUMCITS(INN)
            IHR=IDHURCITY(INN,IDD)
            VHRU=VEHURCITY(INN,IDD) 
            
       FSUCITYS(INN,1)=FSUCITYS(INN,1)+hruARE(IHR)*VHRU
              
       FSUCITYS(INN,2)=FSUCITYS(INN,2)+FSUMHRU(IHR,IY,1)*VHRU  !!!  pcp

       FSUCITYS(INN,3)=FSUCITYS(INN,3)+FSUMHRU(IHR,IY,17)*
     &hruARE(IHR)*1000/86400*VHRU   !!! surfq
 
       FSUCITYS(INN,4)=FSUCITYS(INN,4)+FSUMHRU(IHR,IY,6)*VHRU !!!  et
             
       FSUCITYS(INN,5)=FSUCITYS(INN,5)+VHRU*FSUMHRU(IHR,IY,19) !!TLOSS=TLOSS+FVAR(19)

       FSUCITYS(INN,6)=FSUCITYS(INN,6)+VHRU*FSUMHRU(IHR,IY,20) !!TLATQ=TLATQ+FVAR(20)
       FSUCITYS(INN,7)=FSUCITYS(INN,7)+VHRU*FSUMHRU(IHR,IY,21)  !!GWQ=GWQ+FVAR(21)
       FSUCITYS(INN,8)=FSUCITYS(INN,8)+VHRU*FSUMHRU(IHR,IY,22) !!WYLD=WYLD+FVAR(22)
   
c               FHRUT(IY,9)=(FSUMHRU(IHR,IY,54) 
c     &+FSUMHRU(IHR,IY,55) +FSUMHRU(IHR,IY,57) +FSUMHRU(IHR,IY,51))
c    &*100000/FSUMHRU(IHR,IY,17)   !计算浓度TN
        FSUCITYS(INN,9)=FSUCITYS(INN,9)+VHRU*(FSUMHRU(IHR,IY,54) 
     &+FSUMHRU(IHR,IY,55) +FSUMHRU(IHR,IY,57) +FSUMHRU(IHR,IY,51))*
     &hruARE(IHR)*100   !计算负荷(kg)

	        !!TP_Q=TP_Q+FVAR(52)+FVAR(58)+FVAR(59)
c              FHRUT(IY,10)=FHRUT(IY,10)+FSUMHRU(IHR,IY,52)

         !计算浓度TP
	   FSUCITYS(INN,10)=FSUCITYS(INN,10)+VHRU*(FSUMHRU(IHR,IY,52)    
     &+FSUMHRU(IHR,IY,58) +FSUMHRU(IHR,IY,59))*                    
     &hruARE(IHR)*100   !计算负荷(kg)


              !!!COD=COD+FVAR(23)/(FVAR(2)* 86.4) !!!COD 校核 
         FSUCITYS(INN,11)=FSUCITYS(INN,11)+VHRU* FSUMHRU(IHR,IY,51)/
     &(FSUMHRU(IHR,IY,54)*86.4)   

              !!NH4=NH4+FVAR(57)     !!!氨氮 校核
           FSUCITYS(INN,16)=FSUCITYS(INN,16)+VHRU*FSUMHRU(IHR,IY,57) 
 
           
               !泥沙 
           FSUCITYS(INN,13)=FSUCITYS(INN,29)/hruARE(IHR)*0.01 


	        !!TP_ARE=TP_ARE+TP_Q/AREA
c              FHRUT(IY,14)=FHRUT(IY,15)+FHRUT(IY,10)/hruARE(IHR)*0.01
            FSUCITYS(INN,14)=FSUCITYS(INN,9)/hruARE(IHR)*0.01
            
            FSUCITYS(INN,15)=FSUCITYS(INN,10)/hruARE(IHR)*0.01

       

         END DO
         
          WRITE(240,600)INN,IY,(FSUCITYS(INN,is),is=1,15)
         
         END DO
          
          END DO
           
          close(240) 
      END IF     
      END IF     
      
     
        RETURN
      END

