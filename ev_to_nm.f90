Program ev_to_nm

      Implicit None
     
         Integer, Parameter :: wp = selected_real_kind(12,70)

		Real, Parameter :: h = 6.626068_wp*(10.00_wp**(-34.00_wp))
		Real, Parameter :: c = 2.997924_wp*10.0_wp**(8.0_wp)
                !to the power of 8 as wavelength in nm 
                Real, Parameter :: n = 6.0221415_wp*10.0_wp**23_wp
                Character(3) :: nm = ' nm'
                Character(3) :: eV = ' eV'
                Integer :: i_err
              
        
               Real(wp) :: y, x, k, z

               Write(*,*) 'Enter energy in eV'
               Read(*,*) y


               Open(unit=1, file = 'region.output', status='replace', action='write', iostat=i_err)

               IF (i_err.ne.0) THEN
                       Write(*,*)'Error in writing to file'
                       STOP
               END IF

          x = (y * 96.485_wp * 1000_wp) / n
          k = (h*c) / x
          z = k * 10.0_wp**(9.0_wp)
         
          Write(1,100) y, eV
          Write(1,100) z, nm



       
 IF (y <= 1.6) Then
                       Write(1,*) 'IR'
              ELSE if ((y > 1.6) .and. (y <= 2.0)) then
                      Write(1,*) 'Red region, Material would be Green'
              Else if ((y > 2.0) .and. (y <= 2.14)) then
                      Write(1,*) 'Orange region, Material would be Blue'
              Else if ((y > 2.14) .and. (y <= 2.20)) then
                      Write(1,*) 'Yellow region, Material would be Violet'
              Else if ((y > 2.20) .and. (y <= 2.53)) then
                      Write(1,*) 'Green region, Material would be Red'
              Else if ((y > 2.53) .and. (y <= 2.88)) then
                      Write(1,*) 'Blue region, Material would be Orange'
              Else if ((y > 2.88) .and. (y <= 3.26)) then
                      Write(1,*) 'Violet region, Material would be Yellow'
              Else if (y > 3.26) then
                      Write(1,*) 'This energy is in the UV region or above'
              End IF
 

              Close(Unit=1,status='keep')
             
              
      
100 FORMAT (F10.3,A3)


End program ev_to_nm
