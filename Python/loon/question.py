# #### about l_layer 
# from sklearn.neighbors import KernelDensity

# import pandas as pd
# import numpy as np
# import pylab as pl
# import scipy.stats
# from dataset import iris
# dd =iris.iloc[:,1]
# pdf = scipy.stats.kde.gaussian_kde(dd)
# x = np.linspace((dd.min()-1),(dd.max()+1), len(dd)) 
# y = pdf(x)     


# #### about color
# import seaborn
# seaborn.color_palette()


# class Threader(threading.Thread):
#     def __init__(self, *args, **kwargs):
#         threading.Thread.__init__(self, *args, **kwargs)
#         self.daemon = True
#         self.start()
#     def run(self):
#          while True:
#             print("Look a while true loop that doesn't block the GUI!")
#             print("Current Thread: %s" % self.name)
#             time.sleep(1)

# root = Tk()
# leftFrame = Frame(root)
# leftFrame.pack(side=LEFT)
# rightFrame = Frame(root)
# rightFrame.pack(side=RIGHT)
# playButton = Button(leftFrame, text="Play", fg="blue", 
#     command= lambda: Threader(name='Play-Thread'))
# stopButton = Button(rightFrame, text="Stop", fg="red", 
#     command= lambda: Threader(name='Stop-Thread'))
# playButton.pack(side=TOP)
# stopButton.pack(side=BOTTOM)
# root.mainloop()


# class TimerTest():
#     def __init__(self, root):
#         self.root=root

#         Button(root, text="Play", fg="blue",
#                             command=self.startit).grid(row=1, column=0)
#         Button(root, text="Stop", fg="red",
#                             command=self.stopit).grid(row=1, column=1)

#         self.is_running=True
#         self.count=IntVar()
#         Label(root, textvariable=self.count,
#               bg="lightblue").grid(row=0, column=0, columnspan=2, sticky="ew")

#     def startit(self):
#         self.is_running=True
#         self.increment_counter()

#     def increment_counter(self):
#         if self.is_running:
#              c=self.count.get()
#              c += 1
#              self.count.set(c)
#              root.after(1000, self.increment_counter)  ## every second

#     def stopit(self):
#         self.is_running = False

# root = Tk()
# TT=TimerTest(root)
# root.mainloop()