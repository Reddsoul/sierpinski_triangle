PROGRAM triangle

IMPLICIT NONE
REAL, DIMENSION(3) :: xvert = [0.0, 10.0, 5.0]
REAL, DIMENSION(3) :: yvert = [0.0, 0.0, sqrt(75.0)]
INTEGER :: i, vertex
REAL :: xr, yr, x2, y2, half_xr, half_yr

OPEN(10, FILE='sierpinski_triangle_output.csv')

WRITE(10,*) "x","    ,"," y"

! Initialize random number generator
CALL random_seed()

! Initialize xr randomly
CALL random_number(xr)
xr = xr * 10

! Determine initial yr based on xr
IF (xr > 5.0) THEN
	CALL random_number(yr)
	yr = yr * (17.320508076 - ((yvert(3) / xvert(3)) * xr))
ELSEIF (xr < 5.0) THEN
	CALL random_number(yr)
	yr = yr * (yvert(3) / xvert(3)) * xr
ELSE
	CALL random_number(yr)
	yr = yr * yvert(3)
END IF

! Generate points for the Sierpinski triangle
DO i = 1, 10000
	CALL random_number(x2)
	vertex = INT(x2 * 3) + 1

	! Determine the next point based on the selected vertex
	SELECT CASE (vertex)
	CASE (1)
		x2 = 0.5 * (xvert(1) + xr)
		y2 = 0.5 * (yvert(1) + yr)
	CASE (2)
		x2 = 0.5 * (xvert(2) + xr)
		y2 = 0.5 * (yvert(2) + yr)
	CASE (3)
		x2 = 0.5 * (xvert(3) + xr)
		y2 = 0.5 * (yvert(3) + yr)
	END SELECT

	! Write the new point to the file
	WRITE(10, '(F6.3, ",", F6.3)') x2, y2

	! Update xr and yr for the next iteration
	xr = x2
	yr = y2
END DO

Write(*,*) 'Sierpinski Triangle generated and saved to sierpinski_triangle_ouput.csv'

CLOSE(10)

END PROGRAM triangle