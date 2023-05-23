! 090200133 Tuna Dursun

!   We are asked to create two matrices called matTD and avematTD with the last digit of our student number plus one time nineteen rows and the last
! digit of our student number plus one time twenty-three columns. Filling the matTD matrix using the sum formula given in the question. Next, we fill
! the avematTD matrix with the corresponding elements in the matTD matrix and the arithmetic average of their nearest neighbors (that element and its nearest neighbors).
! Then we add all the elements in the avematTD matrix and take their average. We assign this as averageTD. Finally, we print the averageTD to the screen.
 
program dursunt20
implicit none 

integer,parameter::NrowTD=4*19 
! NrowTD is a constant integer type variable. I created it to calculate the number of rows of the matrices called matTD and avematTD that I will create.
! The last digit of my student number is 3. And the formula is "(student number's last digit + 1)*19".
integer,parameter::NcolTD=4*23
! NcolTD is a constant integer type variable. I created it to calculate the number of columns of the matrices called matTD and avematTD that I will create.
! The last digit of my student number is 3. And the formula is "(student number's last digit + 1)*23".
real,dimension(NrowTD,NcolTD)::matTD,avematTD 
! I have created two matrices called matTD and avematTD and consisting of the real type variable. And the dimension of these matrices is (NrowTD, Ncol,TD).  
real::averageTD ! This is a real type variable. 
integer::i,j !i and j are integer type variables.
integer::kTD=0 ! kTD is an integer type variable and equal to zero.


! With this loop, we calculate each element of matTD one by one with the formula and fill it in the matrix.
do j=1,size(matTD,2) ! It means that j=1 and the loop will continue until j=size(matTD,2). size(matTD,2) means the number of columns of the matrix matTD.
  do i=1,size(matTD,1) ! It means that i=1 and the loop will continue until i=size(matTD,1). size(matTD,1) means the number of rows of the matrix matTD.
    if (i==1 .and. j==1) then ! If i is equal to one and j is equal to one, execute the following code block.
      matTD(i,j)=8.0/((4*kTD+1)*(4*kTD+3))
    else if (i==1 .and. 1<j) then ! If i is equal to one and j is greater than one, execute the following code block.
      matTD(i,j)=8.0/((4*kTD+1)*(4*kTD+3))+matTD(size(matTD,1),j-1)
    else ! If the above conditions are not met, execute the following code block. 
      matTD(i,j)=8.0/((4*kTD+1)*(4*kTD+3))+matTD(i-1,j)
    end if     
    kTD=kTD + 1
  end do
end do  

! With this loop, we calculate each element of avematTD one by one with the algorithm that I created and fill it in the matrix.
do j=1,size(avematTD,2)! It means that j=1 and the loop will continue until j=size(avematTD,2). size(avematTD,2) means the number of columns of the matrix matTD.
  do i=1,size(avematTD,1)! ! It means that i=1 and the loop will continue until i=size(avematTD,1). size(avematTD,1) means the number of rows of the matrix matTD.
    if (1<i .and. i<size(avematTD,1) .and. 1<j .and. j<size(avematTD,2)) then 
    ! If i is greater than one and i is less than size(avematTD,1)and j is greater than one and j is less than size(avematTD,2), execute the following code block.   
        avematTD(i,j)=(matTD(i,j)+matTD(i-1,j)+matTD(i+1,j)+matTD(i,j-1)+matTD(i,j+1))/5.0 
        ! Take the arithmetic mean of the matrix matTD(i,j) itself and its nearest neighbors and assign it to the avematTD(i,j) matrix.
    else if (i==1 .and. j==1) then
    ! If i is equal to one and j is equal to one, execute the following code block.    
       avematTD(i,j)=(matTD(i,j)+matTD(i+1,j)+matTD(i,j+1))/3.0
       ! ! Take the arithmetic mean of the matrix matTD(i,j) itself and its nearest neighbors and assign it to the avematTD(i,j) matrix.
    else if (i==size(avematTD,1) .and. j==size(avematTD,2)) then
    ! If i is equal to size(avematTD,1) and j is equal to size(avematTD,2), execute the following code block.   
       avematTD(i,j)=(matTD(i,j)+matTD(i-1,j)+matTD(i,j-1))/3.0
       ! ! Take the arithmetic mean of the matrix matTD(i,j) itself and its nearest neighbors and assign it to the avematTD(i,j) matrix.
    else if (i==1 .and. j==size(avematTD,2)) then
    ! If i is equal to one and j is equal to size(avematTD,2), execute the following code block.  
       avematTD(i,j)=(matTD(i,j)+matTD(i+1,j)+matTD(i,j-1))/3.0
       ! ! Take the arithmetic mean of the matrix matTD(i,j) itself and its nearest neighbors and assign it to the avematTD(i,j) matrix.
    else if (i==size(avematTD,1) .and. j==1) then
    ! If i is equal to size(avematTD,1) and j is equal to one, execute the following code block.  
       avematTD(i,j)=(matTD(i,j)+matTD(i-1,j)+matTD(i,j+1))/3.0
       ! ! Take the arithmetic mean of the matrix matTD(i,j) itself and its nearest neighbors and assign it to the avematTD(i,j) matrix.
    else if (i==1 .and. 1<j .and. j<size(avematTD,2)) then
    ! If i is equal to one and j is greater than one and j is less than size(avematTD,2), execute the following code block.  
       avematTD(i,j)=(matTD(i,j)+matTD(i+1,j)+matTD(i,j-1)+matTD(i,j+1))/4.0
       ! ! Take the arithmetic mean of the matrix matTD(i,j) itself and its nearest neighbors and assign it to the avematTD(i,j) matrix.
    else if (i==size(avematTD,1) .and. 1<j .and. j<size(avematTD,2)) then
    ! If i is equal to size(avematTD,1) and j is greater than one and j is less than size(avematTD,2), execute the following code block.  
       avematTD(i,j)=(matTD(i,j)+matTD(i-1,j)+matTD(i,j-1)+matTD(i,j+1))/4.0
       ! ! Take the arithmetic mean of the matrix matTD(i,j) itself and its nearest neighbors and assign it to the avematTD(i,j) matrix.
    else if (j==1 .and. 1<i .and. i<size(avematTD,1)) then
    ! If j is equal to one and i is greater than one and i is less than size(avematTD,1), execute the following code block.  
       avematTD(i,j)=(matTD(i,j)+matTD(i-1,j)+matTD(i+1,j)+matTD(i,j+1))/4.0
       ! ! Take the arithmetic mean of the matrix matTD(i,j) itself and its nearest neighbors and assign it to the avematTD(i,j) matrix.
    else if (j==size(avematTD,2) .and. 1<i .and. i<size(avematTD,1)) then
    ! If j is equal to size(avematTD,2) and i is greater than one and i is less than size(avematTD,1), execute the following code block.  
       avematTD(i,j)=(matTD(i,j)+matTD(i-1,j)+matTD(i+1,j)+matTD(i,j-1))/4.0
       ! ! Take the arithmetic mean of the matrix matTD(i,j) itself and its nearest neighbors and assign it to the avematTD(i,j) matrix.
    end if                    
  end do  
end do    

averageTD=sum(avematTD)/(size(avematTD,1)*size(avematTD,2))
! Add all the numbers in the matrix with the sum command and divide this sum by the size(avematTD,1)*size(avematTD,2). Assign the result as averageTD.
print*,"The value of averageTD is:",averageTD ! Print the value of averageTD on the screen.

end program dursunt20
