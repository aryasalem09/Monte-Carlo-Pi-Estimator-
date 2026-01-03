this program estimates the value of pi using the monte carlo method, the math behind is is pretty simple:
  - random points generated inside unit square
  - determine how many of those points lie inside a quater circle of r = 1
  - ratio of points inside the circle to the total points converges to pi/4 as the sample size increases

MSYS2 MINGW64 (for windows atleast)
   - build: ``` gfortran monte_carlo_pi.f90 -O2 -o monte_carlo_pi ```
   - run  : ``` ./monte_carlo_pi ```
enter the number of points and click enter
