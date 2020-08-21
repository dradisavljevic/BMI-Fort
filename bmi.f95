program calculate_bmi
  implicit none
  character :: unit_system
  character :: height_unit*15, weight_unit*10
  real :: height, weight, bmi
  print *, 'Would you like to use imperial or metric system?'
  systemloop: do
    print *, 'Press [I] for imperal system or press [M] for metric system.'
    read *, unit_system
    if (unit_system == 'i' .or. unit_system == 'I' .or. &
     unit_system == 'm' .or. unit_system == 'M') exit systemloop
  end do systemloop

  if (unit_system == 'i' .or. unit_system == 'I') then
    height_unit='inches'
    weight_unit = 'pounds'
  else
    height_unit = 'centimeters'
    weight_unit= 'kilograms'
  end if

  print *, 'Type in your height in ', height_unit
  read *, height
  print *, 'Type in your weight in ', weight_unit
  read *, weight

  if (unit_system == 'i' .or. unit_system == 'I') then
    bmi = 703 * weight / (height**2)
  else
    bmi = weight / ((height/100)**2)
  end if

  print *, 'Your BMI is: '
  write (*, 1) NINT(bmi*10)/10.0
  1 format(f6.1)

  if (bmi < 18.5) then
    print *, 'Judging by your BMI, you are underweight. Please eat something.'
  else if (bmi >= 18.5 .and. bmi < 25) then
    print *, 'Judging by your BMI, you are healthy. Keep it up.'
  else if (bmi >= 25 .and. bmi < 30) then
    print *, 'Judging by your BMI, you are overweight. Be sure to move or exercise a little bit more.'
  else if (bmi >= 30) then
    print *, 'Judging by your BMI, you might be suffering from obesity. Be careful.'
  end if
end program calculate_bmi
