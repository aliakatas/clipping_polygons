import numpy as np
from matplotlib import pyplot as plt

def read_polygon_file(fname):
    '''
    Reads the polygon file given
    its name (and path). 
    Returns a list of (x,y) values for polygon vertices.
    '''
    with open(fname, 'r') as f:
        lines = f.readlines()

    N = int(lines[0])
    if N < 3:
        return []

    points = np.zeros((N,2))

    for i in range(1, N + 1):
        line = lines[i]
        values = line.split()

        points[i - 1][0] = float(values[1])
        points[i - 1][1] = float(values[2])

    return points

def plot_polygon(ax, points, name, color='k'):
    '''
    Draws the polygon from its points on ax axis.
    '''
    if len(points) < 3:
        return 

    pp = points.tolist()
    if points[0][0] != points[-1][0] or points[0][1] != points[-1][1]:
        pp.append(points[0])
    
    X = [p[0] for p in pp]
    Y = [p[1] for p in pp]
    ax.plot(X, Y, '-', label=name, color=color)
    

if __name__ == '__main__':
    clipping_polygon = './data/clipping_polygon.txt'
    clipping_points = read_polygon_file(clipping_polygon)

    # original_polygon = './data/convex_polygon.txt'
    original_polygon = './data/concave_polygon.txt'
    original_points = read_polygon_file(original_polygon)

    clipped_polygon = './data/clipped_polygon.txt'
    clipped_points = read_polygon_file(clipped_polygon)
    
    # Create the plot
    plt.figure(figsize=(8,8))
    ax = plt.gca()

    plot_polygon(ax, clipping_points, 'clipping', color='r')
    plot_polygon(ax, clipped_points, 'clipped', color = 'g')
    plot_polygon(ax, original_points, 'original')

    plt.legend()
    plt.show()


    
