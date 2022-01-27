# git
git init



git add/rm 路径

git status

git commit -m '加了一些注释'


git log

git checkout alsdjogrijeoqwe的id ./recipe/toufu路径
	checkout 以后直接status commit,不用add了
	一定要加路径

git show asdhfalsjdfla的id

```
$ ssh-keygen -t rsa -C "your_email@youremail.com"   # 在本地添加ssh key
$ cd ~/.ssh
$ cat id_rsa.pub        # 复制里面的key
# 添加到github设置里 begins with "ssh-rsa"之类的
$ ssh -T git@github.com # 验证是否成功添加ssh key

$ git config --global user.name "your name"
$ git config --global user.email "your_email@youremail.com"
```
```
$ git remote add origin  <REMOTE_URL> 
# Sets the new remote

$ git remote -v
# Verifies the new remote URL
origin	https://github.com/amidos2006/gym-pcgrl.git (fetch)
origin	https://github.com/amidos2006/gym-pcgrl.git (push)

$  git remote set-url origin https://github.com/JiangZehua/gym-pcgrl.git
# change the remote URL

$ git push origin master
$ git push origin <branch>
```

git push origin master 推送改动

```
$ git branch     # list all the branches
$ git branch <branch name>            # 创建新的branch
$ git checkout -b <branch name>       # 创造并切换branch 
$ git checkout <branch name>          # switch branch
$ git branch -d <branch name>         # delete branch
```

```
git merge <branch name>
```

```
### 1.重命名本地分支
git branch -m new-name  #如果当前在要重命名的分支
git branch -m old-name new-name #如果当前不在要重命名的分支

### 2.删除远程旧名称分支并且push新名称分支
git push origin :old-name new-name

### 3.关联新名称的本地分支和远程分支
 git push origin -u new-name
```

```
### 分别查看仓库 local global system 的配置信息
git config --local --list
git config --global --list
git config --system --list
```

# 打开未知开发者app
`sudo spctl --master-disable`
# Carla 记录

-----------
# Markdown 语法
# 这是一级标题
## 这是二级标题
### 这是三级标题
#### 这是四级标题
##### 这是五级标题
###### 这是六级标题

**这是加粗的文字**
*这是倾斜的文字*`
***这是斜体加粗的文字***
~~这是加删除线的文字~~

>这是引用的内容 1
>>这是引用的内容 2
>>>这是引用的内容 3

分割线
三个或者三个以上的 - 或者 * 都可以
---
****


![图片alt](图片地址 ''图片title'')

图片alt就是显示在图片下面的文字，相当于对图片内容的解释。
图片title是图片的标题，当鼠标移到图片上时显示的内容。title可加可不加

![blockchain](https://ss0.bdstatic.com/70cFvHSh_Q1YnxGkpoWK1HF6hhy/it/u=702257389,1274025419&fm=27&gp=0.jpg "区块链")



[超链接名](超链接地址 "超链接title")
title可加可不加

[简书](http://jianshu.com)
[百度](http://baidu.com)



表格

姓名|技能|排行
--|:--:|--:
刘备|哭|大哥
关羽|打|二哥
张飞|骂|三弟




- 列表内容
+ 列表内容
* 列表内容
注意：- + * 跟内容之间都要有一个空格

1. 列表内容
2. 列表内容
3. 列表内容

注意：序号跟内容之间要有空格
列表嵌套
上一级和下一级之间敲三个空格即可



单行代码：代码之间分别用一个反引号包起来

`代码内容`

代码块
```
  代码...
  代码...
  代码...
```
--------
# 远程服务器使用

## zfw3090

位置
```
/mnt/date/zhaofuwei/Extraroj
```

环境
```
conda activate RL
```

传输文件
```
rsync -aP /Users/jiangzehua/Downloads/CARLA_0.8.2.tar zfw3090:/mnt/date/zhaofuwei/ExtraProj/carla8/
```


查看已安装 
```
apt list --installed
```

解压tar.gz
```
tar -xzvf
```

---
# 终端复用工具 tmux
最简易操作流程

1. 新建会话tmux new -s my_session。
2. 在 Tmux 窗口运行所需的程序。
3. 按下快捷键Ctrl+b d将会话分离。
4. 下次使用时，重新连接到会话tmux attach-session -t my_session。
---

1. 安装（ubantu）

`sudo apt-get install tmux`

2. 新建一个指定名称的会话：

`tmux new -s <session-name>`

也可直接输入tmux启动一个会话，默认是编号0，往后依此类推。

3. 分离会话
即暂时离开tmux会话回到本地窗口。快捷键Ctrl+b d或输入以下命令，就会回到本地窗口

`tmux detach`

4. 查看当前所有的Tmux会话
在本地窗口输入tmux ls命令可以查看当前所有Tmux会话。

5. 接入会话
即从当前窗口进入后台的Tmux会话，

`tmux attach -t <sessiong-name>`

6. 杀死会话

`tmux kill-session -t <session-name>`

7. 切换会话

`tmux switch -t <session-name>`

8. 重命名会话
快捷键：Ctrl+b $，或输入命令：

`tmux rename-session -t <old-name> <new-name>`

9. 其他命令：

 列出所有快捷键，及其对应的 Tmux 命令\
$ tmux list-keys

 列出所有 Tmux 命令及其参数\
$ tmux list-commands

 列出当前所有 Tmux 会话的信息\
$ tmux info

 重新加载当前的 Tmux 配置\
$ tmux source-file ~/.tmux.conf


# 下载工具 wget

# 文件传输工具 rsync scp


# vim
进入命令模式
```
vi filename
```
### 命令模式
i 切换到输入模式，以输入字符。\
x 删除当前光标所在处的字符。\
: 切换到底线命令模式，以在最底一行输入命令。

### 进入输入模式
```
i
```
### 输入模式
正常输入\
ESC，退出输入模式，切换到命令模式



### 底线命令模式
```
:
```

退出程序`q`\
保存文件`w`

## 使用 vi/vim 进入一般模式
如果你想要使用 vi 来建立一个名为 runoob.txt 的文件时，你可以这样做：
```
$ vim runoob.txt
```
左下角有文件名\
在一般模式之中，只要按下 i, o, a 等字符就可以进入输入模式了！

在编辑模式当中，你可以发现在左下角状态栏中会出现 ` –INSERT- ` 的字样，那就是可以输入任意字符的提示。

按下 ` ESC` 按钮回到一般模式\
在一般模式中按下 `:wq `保存后离开 vi

# Linux apt

## 查看已经安装的包
`apt list --installed`



# Carla

## 版本 0.9.10 dev
下载 tar.gz 安装包
`wget https://carla-releases.s3.eu-west-3.amazonaws.com/Linux/CARLA_0.9.10.tar.gz`

解压
`tar -xzvf CARLA_0.9.10.tar.gz`


Run xserver: sudo X :0 &
Run CARLA: DISPLAY=:0.GPU_ID SDL_VIDEODRIVER=x11 ./CarlaUE4.sh -vulkan


## 版本 0.8.2 stable
下载tar安装包


下载pip\
`sudo apt-get install python-pip`

Use pip to install all the CARLA requirements\
`pip install -r PythonClient/requirements.txt`

See the options on 'manual_control.py' with the '--help' command\
`python PythonClient/manual_control.py --help`

启动
`./CarlaUE4/sh /Game/Maps/Town01 -carla-server -benchmark -fps=15 -windowed -ResX=800 -ResY=600`

acts like a server: `-carla-server`\
run at a fixed time-step of 1/15 seconds:`-benchmark -fps=15`\
a window screen with a custom visualization resolution:`-windowed -ResX=800 -ResY=600`\

# Mujoco
## 安装 
官网（https://www.roboti.us/index.html）下载相应平台的 product（如 mujoco200 macos），解压到 ~/.mujoco 目录
```
$ mkdir ~/.mujoco 
$ cp mujoco200_macos.zip ~/.mujoco 
$ cd ~/.mujoco 
$ unzip mujoco200_macos.zip
```

拷贝 mjkey.txt 文件
```
$ cp mjkey.txt ~/.mujoco 
$ cp mjkey.txt ~/.mujoco/mujoco200_macos/bin
```

添加环境变量（~/.zshrc 或 ~/.bashrc）
```
export LD_LIBRARY_PATH=~/.mujoco/mujoco200_macos/bin${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}} 
export MUJOCO_KEY_PATH=~/.mujoco${MUJOCO_KEY_PATH}
```

测试
```
$ cd ~/.mujoco/mujoco200_macos/bin 
$ ./simulate ../model/humanoid.xml
```


由于Mac自带的clang编译器不支持openmp，也就是不支持编译安装mujoco-py，所以要先安装：
```
brew install llvm
brew install boost
brew install hdf5
```

并添加环境变量：
```
# Add this to your .bashrc/.zshrc:
export PATH="/usr/local/opt/llvm/bin:$PATH"

export CC="/usr/local/opt/llvm/bin/clang"
export CXX="/usr/local/opt/llvm/bin/clang++"
export CXX11="/usr/local/opt/llvm/bin/clang++"
export CXX14="/usr/local/opt/llvm/bin/clang++"
export CXX17="/usr/local/opt/llvm/bin/clang++"
export CXX1X="/usr/local/opt/llvm/bin/clang++"

export LDFLAGS="-L/usr/local/opt/llvm/lib"
export CPPFLAGS="-I/usr/local/opt/llvm/include"

```

最新的mujoco 2.0.2.9会出问题，所以安装pip install mujoco_py==2.0.2.8，参考github issue https://github.com/openai/mujoco-py/issues/492





# 解决openGL的问题
初始报错
(gym 装在桌面项目里)
```
Traceback (most recent call last):
  File "/Users/jiangzehua/Desktop/项目/OpenAIBaseline/gym/gym/envs/classic_control/rendering.py", line 27, in <module>
    from pyglet.gl import *
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/__init__.py", line 95, in <module>
    from pyglet.gl.lib import GLException
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/lib.py", line 147, in <module>
    from pyglet.gl.lib_agl import link_GL, link_GLU, link_AGL
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/lib_agl.py", line 43, in <module>
    gl_lib = pyglet.lib.load_library(framework='/System/Library/Frameworks/OpenGL.framework')
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/lib.py", line 124, in load_library
    return self.load_framework(kwargs['framework'])
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/lib.py", line 279, in load_framework
    raise ImportError("Can't find framework %s." % path)
ImportError: Can't find framework /System/Library/Frameworks/OpenGL.framework.

During handling of the above exception, another exception occurred:

Traceback (most recent call last):
  File "test.py", line 9, in <module>
    env.render('human')
  File "/Users/jiangzehua/Desktop/项目/OpenAIBaseline/gym/gym/core.py", line 295, in render
    return self.env.render(mode, **kwargs)
  File "/Users/jiangzehua/Desktop/PCGRL/gym-pcgrl/gym_pcgrl/envs/pcgrl_env.py", line 168, in render
    from gym.envs.classic_control import rendering
  File "/Users/jiangzehua/Desktop/项目/OpenAIBaseline/gym/gym/envs/classic_control/rendering.py", line 35, in <module>
    """
ImportError: 
    Error occurred while running `from pyglet.gl import *`
    HINT: make sure you have OpenGL installed. On Ubuntu, you can run 'apt-get install python-opengl'.
    If you're running on a server, you may need a virtual frame buffer; something like this should work:
    'xvfb-run -s "-screen 0 1400x900x24" python <your_script.py>'
```
### 解决：重装gym
```
>>>pip uninstall gym
>>>pip install gym

>>>python -m pip install gym==0.16.0
```
由于他们的repo版本卡在TensorFlow 1.4， gym 0.15  所以得装一样的

依然报错：
```
Traceback (most recent call last):
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/gym/envs/classic_control/rendering.py", line 27, in <module>
    from pyglet.gl import *
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/__init__.py", line 95, in <module>
    from pyglet.gl.lib import GLException
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/lib.py", line 147, in <module>
    from pyglet.gl.lib_agl import link_GL, link_GLU, link_AGL
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/lib_agl.py", line 43, in <module>
    gl_lib = pyglet.lib.load_library(framework='/System/Library/Frameworks/OpenGL.framework')
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/lib.py", line 124, in load_library
    return self.load_framework(kwargs['framework'])
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/lib.py", line 279, in load_framework
    raise ImportError("Can't find framework %s." % path)
ImportError: Can't find framework /System/Library/Frameworks/OpenGL.framework.

During handling of the above exception, another exception occurred:

Traceback (most recent call last):
  File "test.py", line 9, in <module>
    env.render('human')
  File "/Users/jiangzehua/Desktop/PCGRL/gym-pcgrl/gym_pcgrl/envs/pcgrl_env.py", line 168, in render
    from gym.envs.classic_control import rendering
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/gym/envs/classic_control/rendering.py", line 34, in <module>
    ''')
ImportError: 
    Error occurred while running `from pyglet.gl import *`
    HINT: make sure you have OpenGL install. On Ubuntu, you can run 'apt-get install python-opengl'.
    If you're running on a server, you may need a virtual frame buffer; something like this should work:
    'xvfb-run -s "-screen 0 1400x900x24" python <your_script.py>'
```



尝试import pyglet
```
>>> import pyglet
>>> from pyglet.gl import *
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/__init__.py", line 95, in <module>
    from pyglet.gl.lib import GLException
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/lib.py", line 147, in <module>
    from pyglet.gl.lib_agl import link_GL, link_GLU, link_AGL
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/lib_agl.py", line 43, in <module>
    gl_lib = pyglet.lib.load_library(framework='/System/Library/Frameworks/OpenGL.framework')
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/lib.py", line 124, in load_library
    return self.load_framework(kwargs['framework'])
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/lib.py", line 279, in load_framework
    raise ImportError("Can't find framework %s." % path)
ImportError: Can't find framework /System/Library/Frameworks/OpenGL.framework.
>>> 
```
报错

尝试重装pyglet
```
>>>python -m pip uninstall pyglet
Found existing installation: pyglet 1.5.0
Uninstalling pyglet-1.5.0:
  Would remove:
    /opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet-1.5.0.dist-info/*
    /opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/*
Proceed (y/n)? y
  Successfully uninstalled pyglet-1.5.0
```

```
>>>python -m pip install pyglet 
Collecting pyglet
  Downloading pyglet-1.5.21-py3-none-any.whl (1.1 MB)
     |████████████████████████████████| 1.1 MB 6.0 MB/s 
ERROR: gym 0.16.0 has requirement pyglet<=1.5.0,>=1.4.0, but you'll have pyglet 1.5.21 which is incompatible.
ERROR: baselines 0.1.6 has requirement gym<0.16.0,>=0.15.4, but you'll have gym 0.16.0 which is incompatible.
Installing collected packages: pyglet
Successfully installed pyglet-1.5.21
```

重装了TensorFlow
```
>>>pip install tensorflow==1.15  
```
重装成功 
但依然有这个问题

```
Traceback (most recent call last):
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/gym/envs/classic_control/rendering.py", line 27, in <module>
    from pyglet.gl import *
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/__init__.py", line 95, in <module>
    from pyglet.gl.lib import GLException
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/lib.py", line 147, in <module>
    from pyglet.gl.lib_agl import link_GL, link_GLU, link_AGL
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/gl/lib_agl.py", line 43, in <module>
    gl_lib = pyglet.lib.load_library(framework='/System/Library/Frameworks/OpenGL.framework')
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/lib.py", line 124, in load_library
    return self.load_framework(kwargs['framework'])
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/pyglet/lib.py", line 279, in load_framework
    raise ImportError("Can't find framework %s." % path)
ImportError: Can't find framework /System/Library/Frameworks/OpenGL.framework.

During handling of the above exception, another exception occurred:

Traceback (most recent call last):
  File "test.py", line 9, in <module>
    env.render('human')
  File "/Users/jiangzehua/Desktop/PCGRL/gym-pcgrl/gym_pcgrl/envs/pcgrl_env.py", line 168, in render
    from gym.envs.classic_control import rendering
  File "/opt/anaconda3/envs/optrl/lib/python3.7/site-packages/gym/envs/classic_control/rendering.py", line 34, in <module>
    ''')
ImportError: 
    Error occurred while running `from pyglet.gl import *`
    HINT: make sure you have OpenGL install. On Ubuntu, you can run 'apt-get install python-opengl'.
    If you're running on a server, you may need a virtual frame buffer; something like this should work:
    'xvfb-run -s "-screen 0 1400x900x24" python <your_script.py>'
```



PyopenGL已经装了：
```
>>>python -m pip install PyOpenGL
Requirement already satisfied: PyOpenGL in /opt/anaconda3/envs/optrl/lib/python3.7/site-packages (3.1.0)
```
去看看这个文件夹
```
>>>cd /opt/anaconda3/envs/optrl/lib/python3.7/site-packages
```
里面有这个环境的所有包
找这个文件夹：/System/Library/Frameworks/OpenGL.framework.
因为说`ImportError: Can't find framework /System/Library/Frameworks/OpenGL.framework`
```
>>>ls /System/Library/Frameworks
```

出来一堆文件

```
>>>ls /System/Library/Frameworks/OpenGL
ls: /System/Library/Frameworks/OpenGL: No such file or directory
```

```
pip3 install PyOpenGL PyOpenGL_accelerate
```
```
>>> pwd
/opt/anaconda3/envs/optrl/lib/python3.7/site-packages
>>>(optrl) jiangzehua@jiangzehuadeMacBook-Pro site-packages % ls PyOpenGL
ls: PyOpenGL: No such file or directory
>>>(optrl) jiangzehua@jiangzehuadeMacBook-Pro site-packages % ls PyOpenGL-3.1.0.dist-info            
INSTALLER       METADATA        RECORD          WHEEL           top_level.txt
```

```
(optrl) jiangzehua@jiangzehuadeMacBook-Pro site-packages % ls OpenGL
AGL                     GLUT                    _null.py                error.py                plugins.py
EGL                     GLX                     _opaque.py              extensions.py           raw
GL                      Tk                      acceleratesupport.py    images.py               version.py
GLE                     WGL                     arrays                  latebind.py             wrapper.py
GLES1                   __init__.py             constant.py             lazywrapper.py
GLES2                   __pycache__             constants.py            logs.py
GLES3                   _bytes.py               contextdata.py          osmesa
GLU                     _configflags.py         converters.py           platform
(optrl) jiangzehua@jiangzehuadeMacBook-Pro site-packages % open .
```


### 网上的办法：
This error is because Big Sur no longer has the OpenGL library nor other system libraries in standard locations in the file system and instead uses a cache. PyOpenGL uses ctypes to try to locate the OpenGL library and it fails to find it. Fixing ctypes in Python so that it will find the library is the subject of this pull request

https://github.com/python/cpython/pull/21241

So a future version of Python should resolve the problem.

To fix it now you can edit PyOpenGL file OpenGL/platform/ctypesloader.py changing line
```
    fullName = util.find_library( name )
```
to
```
    fullName = '/System/Library/Frameworks/OpenGL.framework/OpenGL'
```


找到file OpenGL/platform/ctypesloader.py 
然后command F :util.find
Line 35 :改掉
```
# fullName = util.find_library( name )
fullName = '/System/Library/Frameworks/OpenGL.framework/OpenGL'
```
重装pyglet后解决问题
```
(optrl) jiangzehua@jiangzehuadeMacBook-Pro site-packages % python -m pip uninstall pyglet

(optrl) jiangzehua@jiangzehuadeMacBook-Pro site-packages % python -m pip install pyglet 
```


包的位置：
```
(optrl) jiangzehua@jiangzehuadeMacBook-Pro site-packages % pwd
/opt/anaconda3/envs/optrl/lib/python3.7/site-packages
```
Big Sur  的问题
https://stackoverflow.com/questions/63475461/unable-to-import-opengl-gl-in-python-on-macos


# Java version controll tool: jenv #
```
brew install jenv

echo 'export PATH="$HOME/.jenv/bin:$PATH"' >> ~/.bash_profile

echo 'eval "$(jenv init -)"' >> ~/.bash_profile

echo 'export PATH="$HOME/.jenv/bin:$PATH"' >> ~/.zshrc

echo 'eval "$(jenv init -)"' >> ~/.zshrc
```

Once you are done with above steps restart your terminal application or resource your configuration. To do that you can use the following command (make sure to change the file name based on your shell type):
```
source ~/.zshrc
```
Now be able to run 
```
jenv doctor
```
in terminal to verify your installation of jEnv.

should shows:
```
[OK]	No JAVA_HOME set
[ERROR]	Java binary in path is not in the jenv shims.
[ERROR]	Please check your path, or try using /path/to/java/home is not a valid path to java installation.
	PATH : /usr/local/Cellar/jenv/0.5.4/libexec/libexec:/Users/jiangzehua/.jenv/shims:/Users/jiangzehua/.jenv/bin:/Users/jiangzehua/.jenv/bin:/Users/jiangzehua/.jenv/bin:/opt/anaconda3/envs/optrl/bin:/opt/anaconda3/condabin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/usr/local/go/bin:/Library/Apple/usr/bin
[OK]	Jenv is correctly loaded
```


(But there are a couple of more things you want to do before going to the next step; that’s Run the following commands in the terminal if you are using maven(i.e mvn):)
```
# ensure that JAVA_HOME is correct
jenv enable-plugin export
# make Maven aware of the Java version in use (and switch when your project does)
jenv enable-plugin maven
```
(不过我好像不用了))

## install different verisons of java: ##
```
brew install --cask java
```

Lish all JDK versions:
```
$ /usr/libexec/java_home -V
Matching Java Virtual Machines (2):
    12.0.1 (x86_64) "Oracle Corporation" - "OpenJDK 12.0.1" /Library/Java/JavaVirtualMachines/openjdk-12.0.1.jdk/Contents/Home
    1.8.0_292 (x86_64) "AdoptOpenJDK" - "AdoptOpenJDK 8" /Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home
/Library/Java/JavaVirtualMachines/openjdk-12.0.1.jdk/Contents/Home

```
Now you can add to jEnv each of the JDK you installed by the following command:
```
Syntax: 
jenv add <your_jdk_path>
Example:
jenv add /Library/Java/JavaVirtualMachines/openjdk-14.0.1.jdk/Contents/Home
```

For me:
```
(optrl) jiangzehua@jiangzehuadeMacBook-Pro Evocraft-py % jenv add /Library/Java/JavaVirtualMachines/openjdk-12.0.1.jdk/Contents/Home
/Library/Java/JavaVirtualMachines/openjdk-12.0.1.jdk/Contents/Home
openjdk64-12.0.1 added
12.0.1 added
12.0 added
12 added
(optrl) jiangzehua@jiangzehuadeMacBook-Pro Evocraft-py % jenv add /Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home
/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home
openjdk64-1.8.0.292 added
1.8.0.292 added
1.8 added

```

Now you can add each of the listed JDKs using the above command to jEnv. Once you add all the JDKs to jEnv you can list and see whether all the versions you add are available in the jEnv using the following command.
```
jenv versions
```

jEnv is now ready for use in daily development workflows. Few things you want to know before using jEnv;

## Setting system-wide Java version: ##
```
jenv global 14.0
```
## Setting project-wide Java version: ##
If a specific project needs a different version of Java just hit the following command in the terminal by standing in the directory of that project. jEnv will then create a .java-version file that describes which JDK to use for. This file can safely be checked in so that your whole team runs the same version of Java (if using jEnv of course).
```
jenv local 11
```
## Setting shell instance Java version: ##
If you need to run a different version of Java in your shell(i.e terminal window), you can run the following command.
jenv shell openjdk64-1.8.0.252
Checking current Java Version
```
java -version
```

(From https://chamikakasun.medium.com/how-to-manage-multiple-java-version-in-macos-e5421345f6d0)