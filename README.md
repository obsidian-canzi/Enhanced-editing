**【增强编辑】插件旨在增强Obsidian软件的文本编辑操作，力求人性化、智能化、自动化处理当前页面文字。**

## 📣 功能说明
### ☑️ 转换内部链接
1. 「Alt+Z」 在选文两端添加或去除 \[\[ ]] 符号
![转换内部链接](https://user-images.githubusercontent.com/16410542/161436655-eb4f5ee7-9b7c-44ee-964a-f943e64dec50.gif)
	- 若未选中文本，视光标位置分两种情况处理
		- 若光标处在\[\[链接|文字]]当中，则自动去除外围的链接符号
		- 若光标未处在链接当中，则直接输出\[\[字符
	- 支持批量转换用换行符分隔的被选中的多行文本或用顿号分隔的被选中的多个字符串。
	- 所选文本中如出现 "|[]?\*<>/: 等符号，此功能不会执行。

2. 「Alt+Q」 将选文转换为 \[\[|选文]] 样式，方便选择笔记名

### ☑️ 智能符号
- 「Alt+;」 自动转换、匹配或跳过各种类型的括号符号
- 判断前后字符，要么跳过后半部分引号，要么自动补全相关括号。
- 行首遇特殊字符时的转换（参考Easy Typing插件）
	-  》  → >
	-  、 → /

- 判断前两字符并替换为对应的符号
	-  【（ 或  [( 替换为 〖
	-  （< 或  (< 替换为 〈
	-  （【 或  ([ 替换为 〔
	-  “ 【 或 "[  替换为 『
	-  ‘ 【 或  '[  替换为 「
	-  ……  替换为 ^

- 判断光标以前最近的前括号，会匹配对应的后括号
	- 原文是〖文字|-  按「Alt+;」快捷键后，在光标|处自动出现〗符号，以下同理。
	- 《文字|》〈文字|〉［文字|］｛文字|｝【文字|】〖文字|〗〔文字|〕『文字|』「文字|」
- 判断光标以前最近的Md语法（前半截字符组），会匹配对应的后半截字符组。 
	- [[链接]]  ==高亮==   **加粗** ~~删除~~ %%注释%%

- 原来是 [[文字|]]其它文字，在光标|处按「Alt+;」快捷键，光标会跳到]] 后面
	- 即判断后面为链接、粗体、高亮、删除线等MD语法的后半截字符组时 自动跳过

- 判断当前行内容，如为 Dataview、Query、Mermaid等单词，则转为代码块语法
	- dv → ```dataview```
	- qy  → ```query```
	- mm → ```mermaid```
	- 也支持 js css ph c java py等语言名称缩写


### ☑️ 转换 MarkDown 语法
- 转换同义链接「Alt+Q」：将选文转换为 [[|选文]] 样式后再选择文档
- 转换粗体语法「Alt+C」∶将选文转为或去除 **粗体** 效果
- 转换斜体语法「Alt+X」∶将选文转为或去除 _斜体_ 效果
- 转换行内代码「Alt+D」∶将选文转为或去除 `行内代码` 效果
- 转换删除线「Alt+S」∶将选文转为或去除 ~~删除线~~ 效果
- 转换下划线「Alt+H」∶将选文转为或去除 下划线 效果
- 转换代码块「未设置」∶将选文转为或去除 ```代码块``` 效果
- 转换上标语法「Alt+U」∶将选文转为或去除 上标 效果
- 转换下标语法「Alt+N」∶将选文转为或去除 下标 效果
- 转换待办状态「未设置」：转换选文行首的待办状态，顺序为 -[ x-!?><+] 效果
- 转换挖空「未设置」：将选文转为或去除 {{c1::选文}} 效果
- 转换标题语法「Ctrl+1-6」∶指定或取消当前行文本为N级标题
- 转换文字颜色「Ctrl+Shift+1-5」∶将选文转为或去除 橙红紫蓝青 颜色
- 转换背景颜色「Ctrl+Alt+1-5」∶将选文背景转为或去除 橙红紫蓝青 颜色
- 转换无语法文本「Ctrl+Alt+Z」∶去除选文中所有markdown语法字符
- 获取无语法文本「Ctrl+Alt+C」∶去除选文中的所有markdown语法字符，并写入剪贴板

### ☑️ 增减常见括号
- 【选文】「未设置」：在选文两端添加或去除 【】符号
- （选文）「未设置」：在选文两端添加或去除 （）符号
- 「选文」「未设置」：在选文两端添加或去除 「」符号
- 《选文》「未设置」：在选文两端添加或去除 《》符号

### ☑️ 全局转换操作
- 英转中文标点「未设置」∶将笔记中的英文标点转换为中文标点，如,.?!"等
- 中转英文标点「未设置」∶将笔记中的中文标点转换为英文标点，如，。？！“等
- 转换路径语法「未设置」∶将 c:\\windows 与 [](file:///c:\/windows) 路径语法相互转换
- 简体转为繁体「未设置」：将笔记中的简体汉字转换为繁体汉字
- 繁体转为简体「未设置」：将笔记中的繁体汉字转换为简体汉字

### ☑️ 个性增强功能
- 智能粘贴「Ctrl+Alt+V」∶将复制的Office表格直接粘贴为MarkDown语法表格
- 修复错误语法「未设置」∶修复错误的MD语法，如1。列表、【】（）链接、[[]]()回链等
- 修复意外断行「未设置」∶修复笔记中的意外断行（删除结尾不是句式标点的换行符）
- 搜索当前文本「未设置」：通过搜索面板在当前文档中搜索划选内容。
- 获取时间信息「未设置」∶获取当前行中的时间信息，并控制链接笔记中的视频进行跳转播放
- 获取标注文本「未设置」∶获取标题、高亮、注释及前缀(#标注\批注\反思)等文本内容
- 选择当前整段「未设置」：选择光标所在的当前整段文本。
- 选择当前整句「未设置」：选择光标所在的当前整句（中文）文本。
- 自动设置标题「未设置」∶将选文中的单行文本（末尾非标点或数字）转为标题
- 指定当前文件名「未设置」：划选文字后指定为当前笔记的文件名。
- 嵌入当前网址页面「未设置」∶在行末插入Html代码来嵌入所选网址页面。
- 获取相对路径「未设置」：获取当前笔记在库目录内的相对路径。

### ☑️ 插入或去除空行（空格）
- 编辑区内按下回车补加一次换行（有开关）
- 批量插入空行「Ctrl+Shift+L」∶在划选的文本行或全文中间批量插入空白行
- 批量去除空行「Ctrl+Alt+L」∶批量去除划选文本或全文中的空白行
- 上方插入空行「未设置」∶在当前文本行的上行插入空白行
- 下方插入空行「未设置」∶在当前文本行的下行插入空白行
- 末尾追加空格「未设置」∶在每行文本的末尾追加两个空格
- 去除末尾空格「未设置」∶批量去除每个文本行末尾的空格字符
- 添加中英间隔「未设置」：在正文的汉字与字母之间批量添加空格，如 china 中国。
- 去除所有空格「未设置」：去除正文中所有的全、半角空格


>欢迎向蚕子(QQ:312815311) 提出操作需求和建议，我们为增强编辑功能来共同努力！

## 📣 安装方法
【增强编辑】插件短期内不会上架社区商店

### ☑️ 手动安装：
手动下载最新的压缩包，然后将其中文件(main.js, manifest.json) 解压到 **库目录/.obsidian/plugins/ZH增强编辑** 文件夹即可.

### ☑️ BRAT 插件安装：
- 安装 BRAT 插件： [点此安装](https://github.com/TfTHacker/obsidian42-brat)
- 在 BART 插件设置面板添加： obsidian-canzi/Enhanced-editing

