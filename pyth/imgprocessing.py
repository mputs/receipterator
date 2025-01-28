def venergy(gradx, x1, x2, direction, alpha=.5, beta=1, max_iter = 10000, DEBUG=False):
    #parameters:
    # gradx: gradient in horizontal direction
    # cpoints: x-values for the initial points to look at. first point is at the top, second at the bottom
    # direction: indicating if we look from right to left (direction = -1) or from left to right (direction = 1)
    # alpna: weight for the directional part
    # beta: weight for the gradient
    # max_iter: maximum number of iterations
    
    h = gradx.shape[0]
    w = gradx.shape[1]
    sum_x = h * (h + 1) // 2               
    sum_x2 = h * (h + 1) * (2 * h + 1) // 6
    denominator = h * sum_x2 - sum_x**2

    x1 = float(x1)
    x2 = float(x2)

    epsilon = 1e-3

    for i in range(max_iter):
        Y = np.arange(0,gradx.shape[0],1)
        #determine the direction of the line and assign X
        b = x1;
        a = (x2-x1)/Y[-1]
        X = (a*Y+b).astype("int32")
        grad = gradx[Y,X]
        
        # fit a line through the gradient and estimate the values at the two control points
        # the external energy is -gradient at control points (=l.intercept + l.slope*y)
        # the internal energy is forcing the line in a certain direction
        sum_y = grad.sum()                          
        sum_xy = (Y*grad).sum() 
        slope = (h * sum_xy - sum_x * sum_y) / denominator
        intercept = (sum_y * sum_x2 - sum_x * sum_xy) / denominator       
        x1_delta = -intercept + direction*alpha
        x2_delta = -(intercept + slope*Y[-1]) + direction*alpha        
        # move the line
        x1 += x1_delta
        x2 += x2_delta
        delta = np.abs(x1_delta) + np.abs(x2_delta)
        if delta < epsilon:
            break
    if DEBUG: 
        print(f"contour found after {i} iterations (delta = {delta})");
    return (x1,x2)
