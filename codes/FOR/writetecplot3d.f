      SUBROUTINE writetecplot3d(MA,ISTMP)
      USE MPI
C     VERSION(08/07/91)
C
      INCLUDE 'INC\comdeck.INC'
      INCLUDE 'INC\ARCHIVE.INC'    
      INCLUDE 'INC\PARA.INC'
      INCLUDE 'INC\WVULP.INC'   
      INCLUDE 'INC\TRANSPORT.INC'  
      INCLUDE 'INC\ICNT.INC'      
      INCLUDE 'INC\INDEX.INC'      

      
CW---------------利用八个点控制一个体的方法来做三维文件-----------------
CW-----八个点在生成时要保持一致的方式，可以任一侧面四个点组成一组-------
CW------------剩下的四个点是该侧面的对立面同样顺序的四个点--------------

      
      DIMENSION KE_P(110000,6),L_BOOK(110000,4,6)
      DIMENSION J_P(110000),IJ_P(110000),I_P(110000)
      CHARACTER *3 TMA


      TMIDDLE=TIME-(.5*DTI*DAYI/DEI)


CW    =========输出tecplot可以看得文件==============      
 
 
      WRITE(TMA,'(I3.3)')MA
      IF(ISTMP.EQ.0)THEN
        WRITE(TECIT,'(I4.4)')ITMP    
       OPEN(IUTECP(MA),FORM='formatted',
     &FILE=TRIM(FNAME(18))//"_"//TMA//"_"//TECIT//".DAT")    !gcmdm   
      ELSE
        WRITE(TECIT,'(I5.5)')ITTEC
      OPEN(IUTECP(MA),FORM='formatted',
     &FILE=TRIM(FNAMETECPL(MA))//"_"//TECIT//".DAT")    !gcmdm
      END IF



      !每个步长输出一个文件，1~10个的循环输出
   

      NU_IJEALL=0  
      NU_KE=0  
      NU_IJE=0  

CW-----------NU_KE是总的行数-------------------
CW--------NU_IJE是每个方块的编号---------------
CW-------先求出第一层的,得到NU_KE--------------
CW-------先求出第一层的,得到NU_IJE的四个点-----
      DO I=I_AM1(MA,1), I_AM1(MA,2)
          DO J=J_AM1(MA,1), J_AM1(MA,2)
              IJ=(I-1)*NJ+J
              I_P(IJ)=I
              J_P(IJ)=J
              IF(MFS(I,J).GT.0) THEN
                  NU_KE=NU_KE+1

                  KE_P(IJ,1)=NU_KE
                  IJ_P(NU_KE)=IJ
                  IF(I.LT.I_AM1(MA,2).AND.J.LT.J_AM1(MA,2)) THEN
                      IF(MFS(I,J).NE.0.AND.MFS(I+1,J).NE.0.AND.
     @  MFS(I+1,J+1).NE.0.AND.MFS(I,J+1).NE.0) THEN 
                          NU_IJE=NU_IJE+1
                          L_BOOK(NU_IJE,1,1)=IJ   
                          L_BOOK(NU_IJE,2,1)=IJ+NJ   
                          L_BOOK(NU_IJE,3,1)=IJ+NJ+1   
                          L_BOOK(NU_IJE,4,1)=IJ+1
                      END IF
                  END IF
              END IF
          END DO
      END DO 
      
      
      NU_IJEALL=NU_IJE*(KB-1)

CW------------求每个方体的八个点------------
      DO IK=2,KB
          DO N=1,NU_IJE
             L_BOOK(N,1,IK)=L_BOOK(N,1,IK-1)
             L_BOOK(N,2,IK)=L_BOOK(N,2,IK-1)
             L_BOOK(N,3,IK)=L_BOOK(N,3,IK-1)
             L_BOOK(N,4,IK)=L_BOOK(N,4,IK-1)
          END DO
          
        DO I=I_AM1(MA,1), I_AM1(MA,2)
          DO J=J_AM1(MA,1), J_AM1(MA,2)
              IJ=(I-1)*NJ+J
           
              IF(MFS(I,J).GT.0) THEN
                
                  KE_P(IJ,IK)=KE_P(IJ,IK-1)+NU_KE  
              END IF
          END DO
      END DO
          
      END DO
      
       
      
      WRITE(IUTECP(MA),*)'TITLE="QSH T=',TMIDDLE,'d"'

      WRITE(IUTECP(MA),409)'VARIABLES="X ","Y ","Z ",
     @"U ","V ","W ","T ","ACC ","Q ","ELF ",
     &"ZB ","IA ","JA ","I ","J ","MA ","IBC "'

      WRITE(IUTECP(MA),*)"ZONE N=",NU_KE*KB," E=",NU_IJEALL," 
     & ,DATAPACKING = POINT, ZONETYPE = FEBRICK "    
      
      DO K=1,KB          
      DO IK=1,NU_KE 
          IJ=IJ_P(IK)
          I=I_P(IJ)
          J=J_P(IJ)
          IRL=I0(I,J)
          JRL=J0(I,J) 
          ZBB=ELF(I,J)+Z(K)*DT(I,J)
          WRITE(IUTECP(MA),209)X0(I,J),Y0(I,J),ZBB,
     &U(I,J,K),V(I,J,K),W(I,J,K),T(I,J,K),ARCC1(I,J,K),
     &XMFL3D(I,J,K),ELF(I,J),ZB(I,J)
     &,IRL,JRL,I,J,MA,IBOUN(I,J)
      END DO
      END DO
      
      DO K=1,KB-1   
      DO IB=1,NU_IJE 
          WRITE(IUTECP(MA),309)(KE_P(L_BOOK(IB,IL,K),K),IL=1,4),
     &(KE_P(L_BOOK(IB,IL,K+1),K+1),IL=1,4)
      END DO
      END DO 
      
      CLOSE(IUTECP(MA))


!!      call MPI_Barrier(MPI_COMM_WORLD,IERR) 
      
      CALL writeresall3d(MA,ISTMP)
 
      
 209  FORMAT(2F18.2,9F14.5,6I10)   
 309  FORMAT(8I8)   
 409  FORMAT(A185)   


      RETURN
      END

