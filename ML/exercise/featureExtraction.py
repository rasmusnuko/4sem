import math
# Initialize
q = [(1/16), (8/16), (7/16)]
a = [(1/9), (4/9), (4/9)]
b = [(8/16), (1/16), (7/16)]
c = [(2/16), (4/16), (10/16)]
d = [(1/16), (2/16), (13/16)]
vectors = [a,b,c,d]

# Find Euclidean distances
distances = []
for vec in vectors:
    distances.append(math.sqrt( (q[0]-vec[0])**2 + (q[1]-vec[1])**2 + (q[2]-vec[2])**2))

# Print results
results = ["a","b","c","d"]
results = zip(results, distances)
for result in results:
    print("Vector:", result[0], "distance:", result[1])

