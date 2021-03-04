u = [2,4,6]                         # centroid1, centroid2, centroid3
uPoints = []                        # points belonging to centroid1, centroid2, centroid3
points = [2,3,4,10,11,12,20,25,30]  # Our data
print("\n=== Initialization ===")
for x in range(len(u)):
    print(f'Centroid {x+1}: {u[x]}')
change = True                       # True iff centroids change
iteration = 0
while(change):
    iteration += 1
    print("\n=== Iteration:", iteration, "===")
    change = False
    
    # Reset uPoints
    uPoints = []
    for x in range(len(u)):
        uPoints.append([])

    # Assign points to centroids
    lowestDist = -1
    for point in points:
        min = 35
        for x in range(len(u)):
            if min > abs(u[x] - point):
                min = abs(u[x] - point)
                lowestDist = x
        uPoints[lowestDist].append(point)
    
    for x in range(len(u)):
        # Print values
        print(f'Centroid {x+1}: {u[x]} | Points: {uPoints[x]}')

        # Calculate new centroids
        # Check if centroids change
        oldValue = u[x]
        u[x] = sum(uPoints[x])/len(uPoints[x])
        u[x] = float("%0.2f" % u[x])
        if oldValue != u[x]:
            change = True