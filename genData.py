import numpy as np
import matplotlib.pyplot as plt


def ackley_fun(x,y):
	return -20*np.exp(-0.2*np.sqrt((x**2+y**2)/2))*-np.exp((np.cos(2*np.pi*x)+np.cos(2*np.pi*y))/2)+20+np.e

def beale_fun(x,y):
	return (1.5-x*(1-y))**2+(2.25-x*(1-y**2))**2+(2.625-x*(1-y**3))**2
	
def booth_fun(x,y):
	return (x+2*y-7)**2 + (2*x+y-5)**2

def de_jong_fun(x,y):
	return x**2 + y**2

def rosenbrock_fun(x,y):
	return 100*(y-x**2)**2 + (1-x)**2

def himmelblau_fun(x,y):
	return (x**2 + y - 11)**2 + (x + y**2 - 7)**2

def griewangk_fun(x,y):
	return (x**2)/4000+(y**2)/4000 - np.cos(x)*np.cos(y/np.sqrt(2)) + 1

print(ackley_fun(-4.7959185, -5.0))
#print(griewangk_fun(1,4))
#exit()

res = []
lB = -5
uB = 5
num_samples = 50
x_vals = np.linspace(lB,uB,num_samples)
y_vals = np.linspace(lB,uB,num_samples)
X,Y = np.meshgrid(x_vals,y_vals)
Z_rosen = rosenbrock_fun(X,Y)
Z_ackley = ackley_fun(X,Y)
Z_booth = booth_fun(X,Y)
Z_him =  himmelblau_fun(X,Y)
Z_dej = de_jong_fun(X,Y)
Z_beale = beale_fun(X,Y)
Z_griewangk = griewangk_fun(X,Y)


from mpl_toolkits.mplot3d.axes3d import Axes3D
fig = plt.figure(figsize=(14,6))

ax = fig.add_subplot(1, 1, 1, projection='3d')
ax.plot_surface(X, Y, Z_ackley, rstride=4, cstride=4, linewidth=0)
exit()
ax = fig.add_subplot(7, 1, 2, projection='3d')
ax.plot_surface(X, Y, Z_beale, rstride=4, cstride=4, linewidth=0)
ax = fig.add_subplot(7, 1, 3, projection='3d')
ax.plot_surface(X, Y, Z_booth, rstride=4, cstride=4, linewidth=0)
ax = fig.add_subplot(7, 1, 4, projection='3d')
ax.plot_surface(X, Y, Z_dej, rstride=4, cstride=4, linewidth=0)
ax = fig.add_subplot(7, 1, 5, projection='3d')
ax.plot_surface(X, Y, Z_griewangk, rstride=4, cstride=4, linewidth=0)
ax = fig.add_subplot(7, 1, 6, projection='3d')
ax.plot_surface(X, Y, Z_him, rstride=4, cstride=4, linewidth=0)
ax = fig.add_subplot(7, 1, 7, projection='3d')
ax.plot_surface(X, Y, Z_rosen, rstride=4, cstride=4, linewidth=0)
#plt.show()


s = open('samples.txt','w')
s.write('(')
for i in range(num_samples):
	for j in range(num_samples):
		s.write('({0} {1}) '.format(X[i][j],Y[i][j]))
s.write(')')
s.close()

f = open('ackley_fun.txt','w')
f.write('(')
for i in range(num_samples):
	for j in range(num_samples):
		f.write('{0} '.format(Z_ackley[i][j]))
f.write(')')
f.close

f = open('beale_fun.txt','w')
f.write('(')
for i in range(num_samples):
	for j in range(num_samples):
		f.write('{0} '.format(Z_beale[i][j]))
f.write(')')
f.close

f = open('booth_fun.txt','w')
f.write('(')
for i in range(num_samples):
	for j in range(num_samples):
		f.write('{0} '.format(Z_booth[i][j]))
f.write(')')
f.close

f = open('de_jong_fun.txt','w')
f.write('(')
for i in range(num_samples):
	for j in range(num_samples):
		f.write('{0} '.format(Z_dej[i][j]))
f.write(')')
f.close

f = open('griewangk_fun.txt','w')
f.write('(')
for i in range(num_samples):
	for j in range(num_samples):
		f.write('{0} '.format(Z_griewangk[i][j]))
f.write(')')
f.close

f = open('himmelblau_fun.txt','w')
f.write('(')
for i in range(num_samples):
	for j in range(num_samples):
		f.write('{0} '.format(Z_him[i][j]))
f.write(')')
f.close

f = open('rosenbrock_fun.txt','w')
f.write('(')
for i in range(num_samples):
	for j in range(num_samples):
		f.write('{0} '.format(Z_rosen[i][j]))
f.write(')')
f.close