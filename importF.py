__author__ = 'trnkatomas'
import numpy as np
import matplotlib.pyplot as plt
import os
from mpl_toolkits.mplot3d import *
from mpl_toolkits.mplot3d.axes3d import Axes3D

#functions = ['de_jong']

def process_fitness_plot(path,finess_file_name,k):
    data = []
    for i in range(0,100):#100):
        file = open(os.path.join(path,"{0}-{1}.txt".format(fitness_file_template,i)),'r')
        text = file.read()
        file.close()
        values = text.split()
        float_val = [float(x) for x in values]
        float_val = np.asarray(float_val)
        data.append(float_val)
    #data = data.asType(float)
    plt.boxplot(data, sym='')
    plt.xticks(np.arange(0, 100+1, 10))
    plt.savefig(os.path.join(path,'{0}-{1}.png'.format(fitness_file_template,k)))
    plt.show()

def process_function(text):
    text = text.strip()
    if text.find(' ')<0:
        if len(text) <= 1:
            return text
        else:
            try:
                return int(text)
            except:
                print("Some fatal error during parsing function")
        #return text
    text = text[1:len(text)-1]
    op = ""
    if text[0]=="+" or text[0]=="-" or text[0]=="*" or text[0]=="/":
        op = text[0]
        new_text = text[1:]
        counter = 0
        start = 0
        end = 0
        if new_text.find('(') < 0:
            t = new_text.split()
            return "{0} {1} {2}".format(process_function(t[0]),op,process_function(t[1]))
        else:
            for i in range(0,len(new_text)):
                if new_text[i]=="(":
                    if counter==0:
                        start = i
                    counter = counter + 1
                elif new_text[i]==")":
                    counter = counter - 1
                else:
                    if new_text[i].isspace():
                        continue
                    if counter==0:
                        return "({0}) {1} ({2})".format(process_function(new_text[1:new_text.find('(')]),op,process_function(new_text[new_text.find('('):]))
                    else:
                        continue
                if counter==0:
                    end = i
                    return "({0}) {1} ({2})".format(process_function(new_text[start-1:end+1]),op,process_function(text[end+2:]))
    if text[0]=="C" or text[0]=="S" or text[0]=="T" or text[0]=="E":
        op = text[0:3]
        new_text = text[4:]
        counter = 0
        start = 0
        end = 0
        if new_text.find('(') < 0:
            t = new_text.strip()
            return "np.{0}({1})".format(op.lower(),process_function(t))
        else:
            for i in range(0,len(new_text)):
                if new_text[i]=="(":
                    if counter==0:
                        start = i
                    counter = counter + 1
                elif new_text[i]==")":
                    counter = counter - 1
                else:
                    continue
                if counter==0:
                    end = i
                    return "(np.{0}({1}))".format(op.lower(),process_function(new_text[start:end+1]))

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

functions = ['ackley','beale','booth','de_jong','griewangk','himmelblau']#,'rosenbrock']
folds = range(1,11)
#folds = range(2,8)
#folds = range(8,11)
functions_max = {}
for i in functions:
    functions_max[i] = [-1,10000000] 

import re

for k in folds:
    if k==3:
        continue
    for fnc in functions:
        file_template = "res-evo-{0}_fun-99.txt".format(fnc)
        fitness_file_template = "fitness-{0}_fun".format(fnc)
        folder_path = os.path.dirname(os.path.realpath(__file__))
        folder_path = os.path.join(folder_path,"cross-val-{0}".format(k))
        #process_fitness_plot(folder_path, fitness_file_template,k)
        f = open(os.path.join(folder_path,"{1}".format(k,file_template)),'r')
        text = f.read()
        function = text[0:text.rfind(')')+1]
        valueT = text[text.rfind(')')+1:]
        value = valueT[:valueT.rfind('\n')].strip()
        print(fnc)
        value = float(re.sub("[^0-9.]", "", value))
        print(value)
        if functions_max[fnc][1]>value:
            functions_max[fnc] = [k,value,function]            
        continue
        pf = process_function(function)
        print(pf)
        fun = """def my_{0}(X,Y):
                    return {1}""".format(fnc,pf)
        exec(fun)
        [X,Y] = np.meshgrid(np.linspace(-5,5,50),np.linspace(-5,5,50))
        #Z = eval('my_{0}'.format(i),X,Y)
        exec('Z=my_{0}(X,Y)'.format(fnc))
        exec('Z_orig={0}_fun(X,Y)'.format(fnc))
        fig = plt.figure(figsize=(14,6))
        ax = fig.add_subplot(2, 1, 1, projection='3d')
        ax.plot_surface(X, Y, Z, rstride=4, cstride=4, linewidth=0)
        ax = fig.add_subplot(2, 1, 2, projection='3d')
        ax.plot_surface(X, Y, Z_orig, rstride=4, cstride=4, linewidth=0)
        plt.savefig(os.path.join(folder_path,'{0}-{1}.png'.format(fnc,k)))
        plt.show()
for i in functions_max:
    print(i,functions_max[i][:2])
    print(functions_max[i][2])        
#print(functions_max)