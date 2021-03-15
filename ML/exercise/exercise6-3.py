circles = [(1,3), (1,8), (1,9), (4,6), (5,7), (6,8), (7,6)]
squares = [(5,4), (6,1), (6,3), (7,2), (7,4), (8,2), (8,3)]
ks = [4,7,10]
triangle = (6,6)

def dist(p,q):
    # Manhattan dist
    return abs(p[0]-q[0])+abs(p[1]-q[1])

for k in ks:
    list = [None] * k
    distCircles = sorted([dist(circle, triangle) for circle in circles])
    distSquares = sorted([dist(square, triangle) for square in squares])
    distances = [0,0]
    for x in range(k):
        if(distCircles[0] < distSquares[0]):
            distances[0] += 1
            distCircles = distCircles[1:]
        else:
            distances[1] += 1
            distSquares = distSquares[1:]

    if distances[0] > distances[1]:
        print("When k =",k, "The triangle is a circle")
    else:
        print("When k =",k, "The triangle is a square")

