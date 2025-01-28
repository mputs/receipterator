from libc.math cimport abs
import cython
@cython.boundscheck(False)
@cython.wraparound(False)
@cython.nonecheck(False)
@cython.cdivision(True) 
def c_venergy(double[:,::1] gradx, double x1, double x2, int direction, double alpha=.5, double beta=1, int max_iter = 20000, DEBUG=False):
    #parameters:
    # gradx: gradient in horizontal direction
    # cpoints: x-values for the initial points to look at. first point is at the top, second at the bottom
    # direction: indicating if we look from right to left (direction = -1) or from left to right (direction = 1)
    # alpna: weight for the directional part
    # beta: weight for the gradient
    # max_iter: maximum number of iterations
    
    cdef long int h = gradx.shape[0]
    cdef long int w = gradx.shape[1]
    cdef long double sum_y, sum_xy
    cdef long int sum_x = h * (h + 1) // 2               
    cdef long double sum_x2 = h * (h + 1) * (2 * h + 1) // 6
    cdef long double denominator = h * sum_x2 - sum_x**2
    cdef int x, y
    cdef long double grad
    cdef double slope, intercept
    
    epsilon = 1e-3

    for i in range(max_iter):
        #determine the direction of the line and assign X
        b = x1;
        a = (x2-x1)/(h-1)
        
        # fit a line through the gradient and estimate the values at the two control points
        # the external energy is -gradient at control points (=l.intercept + l.slope*y)
        # the internal energy is forcing the line in a certain direction
        sum_y  = 0.0
        sum_xy = 0.0
        for y in range(0,h):
            x = <int>(a*y+b)
            grad = gradx[y,x]
            sum_y += grad                          
            sum_xy += (y*grad) 
        slope = (h * sum_xy - sum_x * sum_y) / denominator
        intercept = (sum_y * sum_x2 - sum_x * sum_xy) / denominator       
        x1_delta = -intercept + direction*alpha
        x2_delta = -(intercept + slope*(h-1)) + direction*alpha        
        # move the line
        x1 += x1_delta
        x2 += x2_delta
        delta = abs(x1_delta) + abs(x2_delta)
        if delta < epsilon:
            break
    if DEBUG: 
        print(f"contour found after {i} iterations (delta = {delta})");
    return (x1,x2)
