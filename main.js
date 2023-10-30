'use strict';

var obsidian = require('obsidian');

/* *****************************************************************************
使用声明
ZH增强编辑插件借鉴多款社区插件开发而成，蚕子水平有限，代码或有缺陷，不能保证任何操作均会正常，请在使用之前备份库笔记，谢谢配合。
开发：蚕子 QQ：312815311 更新时间：2023-10-31
***************************************************************************** */


const 当前版本 = '0.6.4';
const 功能更新 = '当前版本\n- 增加「标签双链互转 Ctrl+Alt+Shift+3」功能\n- 增加按ESC键关闭格式刷功能 \n- 优化功能菜单项及说明文档 \n- 补充修复外来文本功能 \n- 修正插件功能的说明文字（感谢 disjuno） \n- 增加一、二级标题格式刷功能 \n- 去除插入制表符「Tab」功能 \n- 优化「列表转图示」功能  \n- 增加「获取当前字符数」功能 \n- 增加「插入有效空行」功能 \n- 增加「空格转为空行」功能 \n- 状态栏显示Markdown及Html语法格式刷  \n- 状态栏显示实用功能菜单 \n- 增加「获取搜索结果（列表）」功能 \n- ...';
const 宣传页面 = '查看 <a href="https://github.com/obsidian-canzi/Enhanced-editing/releases">Github 页面</a>，联系<a href="http://wpa.qq.com/msgrd?v=3&uin=312815311&site=qq&menu=yes">蚕子</a>';
const 上标图标 ='<svg xmlns="http://www.w3.org/2000/svg" stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24"><path fill="currentColor"d="M16 7.41L11.41 12L16 16.59L14.59 18L10 13.41L5.41 18L4 16.59L8.59 12L4 7.41L5.41 6L10 10.59L14.59 6L16 7.41M21.85 9h-4.88V8l.89-.82c.76-.64 1.32-1.18 1.7-1.63c.37-.44.56-.85.57-1.23a.884.884 0 0 0-.27-.7c-.18-.19-.47-.28-.86-.29c-.31.01-.58.07-.84.17l-.66.39l-.45-1.17c.27-.22.59-.39.98-.53S18.85 2 19.32 2c.78 0 1.38.2 1.78.61c.4.39.62.93.62 1.57c-.01.56-.19 1.08-.54 1.55c-.34.48-.76.93-1.27 1.36l-.64.52v.02h2.58V9z"/></svg>';
const 下标图标 = '<svg xmlns="http://www.w3.org/2000/svg" stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24"><path fill="currentColor" d="M16 7.41L11.41 12L16 16.59L14.59 18L10 13.41L5.41 18L4 16.59L8.59 12L4 7.41L5.41 6L10 10.59L14.59 6L16 7.41m5.85 13.62h-4.88v-1l.89-.8c.76-.65 1.32-1.19 1.7-1.63c.37-.44.56-.85.57-1.24a.898.898 0 0 0-.27-.7c-.18-.16-.47-.28-.86-.28c-.31 0-.58.06-.84.18l-.66.38l-.45-1.17c.27-.21.59-.39.98-.53s.82-.24 1.29-.24c.78.04 1.38.25 1.78.66c.4.41.62.93.62 1.57c-.01.56-.19 1.08-.54 1.55c-.34.47-.76.92-1.27 1.36l-.64.52v.02h2.58v1.35z"/></svg>';
const 格式刷图标 ='<svg t="1650117667147" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="12959" width="120" height="120"><path d="M409.856 331.9296l103.1936-103.168 307.712 307.712-103.168 103.168z" fill="#777677" p-id="12960"></path><path d="M384 358.4s-153.6 128-256 99.84c23.04 38.4 53.76 76.8 51.2 79.36 79.36 17.92 204.8-51.2 204.8-51.2l25.6 25.6s-133.12 102.4-204.8 76.8c66.56 99.84 212.48 225.28 256 256 97.28 0 230.4-179.2 230.4-179.2L384 358.4z" fill="#FCAF6D" p-id="12961"></path><path d="M641.3568 306.9952l153.856-153.856 103.1936 103.168-153.856 153.856z" fill="#777677" p-id="12962"></path></svg>';
const md语法图标 = '<svg t="1650122437266" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="5178" width="120" height="120"><path d="M96 672v-341.333333h85.333333l128 128 128-128h85.333334v341.333333h-85.333334v-220.586667l-128 128-128-128v220.586667h-85.333333m597.333333-341.333333h128v170.666666h106.666667l-170.666667 192-170.666666-192h106.666666z" fill="#42A5F5" p-id="5179"></path></svg>';
const 普通格式刷 ='<svg t="1650117667147" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="12959" width="120" height="120"><path d="M409.856 331.9296l103.1936-103.168 307.712 307.712-103.168 103.168z" fill="#777677" p-id="12960"></path><path d="M384 358.4s-153.6 128-256 99.84c23.04 38.4 53.76 76.8 51.2 79.36 79.36 17.92 204.8-51.2 204.8-51.2l25.6 25.6s-133.12 102.4-204.8 76.8c66.56 99.84 212.48 225.28 256 256 97.28 0 230.4-179.2 230.4-179.2L384 358.4z" fill="#777677" p-id="12961"></path><path d="M641.3568 306.9952l153.856-153.856 103.1936 103.168-153.856 153.856z" fill="#777677" p-id="12962"></path></svg>';
const 全局命令图标 = '<svg t="1650192738325" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="70946" width="110" height="110"><path d="M129.9 755.5625c-34.025 0-64.5 28.625-64.5 62.6875 0 34.0625 30.4625 64.5 64.5 64.5 34 0 62.6875-30.4375 62.6875-64.5C192.5875 784.1875 163.9 755.5625 129.9 755.5625zM129.9 447.4c-34.025 0-64.5 28.65-64.5 62.6875s30.4625 62.8125 64.5 62.8125c34 0 62.6875-28.775 62.6875-62.8125S163.9 447.4 129.9 447.4zM359.1875 259.3375 901.875 259.3375c32.25 0 59.125-25.0875 59.125-57.3125 0-32.25-26.875-59.125-59.125-59.125L359.1875 142.9c-32.275 0-59.125 26.875-59.125 59.125C300.0625 234.25 326.9 259.3375 359.1875 259.3375zM129.9 137.525c-34.025 0-64.5 30.4625-64.5 64.5 0 34 30.4625 62.6875 64.5 62.6875 34 0 62.6875-28.6875 62.6875-62.6875C192.5875 168 163.9 137.525 129.9 137.525zM901.875 451 359.1875 451c-32.275 0-59.125 26.8375-59.125 59.0875s26.8375 59.15 59.125 59.15L901.875 569.2375c32.25 0 59.125-26.9 59.125-59.15S934.125 451 901.875 451zM901.875 759.125 359.1875 759.125c-32.275 0-59.125 26.875-59.125 59.125 0 32.25 26.8375 59.125 59.125 59.125L901.875 877.375c32.25 0 59.125-26.875 59.125-59.125C961 786 934.125 759.125 901.875 759.125z" fill="#1290f8" p-id="70947"></path></svg>';

const DEFAULT_SETTINGS = {
    //isTab: false,
    isBT: false,
    isShowNum: false,
    linkWords:"",
    maxScroll:50,
    version: "0.4.5",
    hColor: "",
    bColor: "",
    hColor1: "#F36208",
    hColor2: "#81B300",
    hColor3: "#2485E3",
    hColor4: "#C32E94",
    hColor5: "#13C6C3",

    bColor1: "#FFB78B",
    bColor2: "#CDF469",
    bColor3: "#A0CCF6",
    bColor4: "#F0A7D8",
    bColor5: "#ADEFEF"
};

var 简体字表 = "皑蔼碍爱肮翱袄奥坝罢摆败颁办绊帮绑镑谤剥饱宝报鲍辈贝钡狈备惫绷笔毕毙币闭边编贬变辩辫标鳖别瘪濒滨宾摈饼并拨钵铂驳卜补财参蚕残惭惨灿苍舱仓沧厕侧册测层诧搀掺蝉馋谗缠铲产阐颤场尝长偿肠厂畅钞车彻尘沉陈衬撑称惩诚骋痴迟驰耻齿炽冲虫宠畴踌筹绸丑橱厨锄雏础储触处传疮闯创锤纯绰辞词赐聪葱囱从丛凑蹿窜错达带贷担单郸掸胆惮诞弹当挡党荡档捣岛祷导盗灯邓敌涤递缔颠点垫电淀凋钓调迭谍叠钉顶锭订丢东动栋冻斗犊独读赌镀锻断缎兑队对吨顿钝夺堕鹅额讹恶饿儿尔饵贰发罚阀珐矾钒烦范贩饭访纺飞诽废费纷坟奋愤粪丰枫锋风疯冯缝讽凤肤辐抚辅赋复负讣妇缚该钙盖干杆赶秆赣冈刚钢纲岗皋镐搁鸽阁铬个给龚宫巩贡钩沟苟构购够蛊顾剐挂关观馆惯贯广规硅归龟闺轨诡柜贵刽辊滚锅国过骇韩汉号阂鹤贺横轰鸿红后壶护沪户哗华画划话怀坏欢环还缓换唤痪焕涣黄谎挥辉毁贿秽会烩汇讳诲绘荤浑伙获货祸击机积饥迹讥鸡绩缉极辑级挤几蓟剂济计记际继纪夹荚颊贾钾价驾歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧将浆蒋桨奖讲酱胶浇骄娇搅铰矫侥脚饺缴绞轿较秸阶节茎鲸惊经颈静镜径痉竞净纠厩旧驹举据锯惧剧鹃绢杰洁结诫届紧锦仅谨进晋烬尽劲荆觉决诀绝钧军骏开凯颗壳课垦恳抠库裤夸块侩宽矿旷况亏岿窥馈溃扩阔蜡腊莱来赖蓝栏拦篮阑兰澜谰揽览懒缆烂滥琅捞劳涝乐镭垒类泪篱狸离里鲤礼丽厉励砾历沥隶俩联莲连镰怜涟帘敛脸链恋炼练粮凉两辆谅疗辽镣猎临邻鳞凛赁龄铃凌灵岭领馏刘龙聋咙笼垄拢陇楼娄搂篓芦卢颅庐炉掳卤虏鲁赂禄录陆驴吕铝侣屡缕虑滤绿峦挛孪滦乱抡轮伦仑沦纶论萝罗逻锣箩骡骆络妈玛码蚂马骂吗买麦卖迈脉瞒馒蛮满谩猫锚铆贸么霉没镁门闷们锰梦眯谜弥觅幂绵缅庙灭悯闽鸣铭谬谋亩呐钠纳难挠脑恼闹馁内拟你腻撵捻酿鸟聂啮镊镍柠狞宁拧泞钮纽脓浓农疟诺欧鸥殴呕沤盘庞抛赔喷鹏骗飘频贫苹凭评泼颇扑铺朴谱栖凄脐齐骑岂启气弃讫牵扦钎铅迁签谦钱钳潜浅谴堑枪呛墙蔷强抢锹桥乔侨翘窍窃钦亲寝轻氢倾顷请庆琼穷趋区躯驱龋颧权劝却鹊确让饶扰绕热韧认纫荣绒软锐闰润洒萨鳃赛叁伞丧骚扫涩杀刹纱筛晒删闪陕赡缮墒伤赏烧绍赊摄慑设绅审婶肾渗声绳胜圣师狮湿诗尸时蚀实识驶势适释饰视试寿兽枢输书赎属术树竖数帅双谁税顺说硕烁丝饲耸怂颂讼诵擞苏诉肃虽随绥岁孙损笋缩琐锁獭挞抬台态摊贪瘫滩坛谭谈叹汤烫涛绦讨腾誊锑题体屉条贴铁厅听烃铜统头秃图涂团颓蜕脱鸵驮驼椭洼袜弯湾顽万网韦违围为潍维苇伟伪纬喂谓卫温闻纹稳问瓮挝蜗涡窝卧呜钨乌污诬无芜吴坞雾务误锡牺袭习铣戏细虾辖峡侠狭厦吓锨鲜纤咸贤衔闲显险现献县馅羡宪线厢镶乡详响项萧嚣销晓啸蝎协挟携胁谐写泻谢锌衅兴凶汹锈绣虚嘘须许叙绪续轩悬选癣绚学勋询寻驯训讯逊压鸦鸭哑亚讶阉烟盐严岩颜阎艳厌砚彦谚验鸯杨扬疡阳痒养样瑶摇尧遥窑谣药爷页业叶一医铱颐遗仪彝蚁艺亿忆义诣议谊译异绎荫阴银饮隐樱婴鹰应缨莹萤营荧蝇赢颖哟拥佣痈踊咏涌优忧邮铀犹游诱于舆鱼渔娱与屿语吁御狱誉预驭鸳渊辕园员圆缘远愿约跃钥岳粤悦阅云郧匀陨运蕴酝晕韵杂灾载攒暂赞赃脏凿枣灶责择则泽贼赠扎札轧铡闸栅诈斋债毡盏斩辗崭栈战绽张涨帐账胀赵蛰辙锗这贞针侦诊镇阵挣睁狰争帧症郑证织职执纸志挚掷帜质滞钟终种肿众诌轴皱昼骤猪诸诛烛瞩嘱贮铸筑注驻专砖转赚桩庄装妆壮状锥赘坠缀谆准着浊兹资渍踪综总纵邹诅组钻锕嗳嫒瑷暧霭谙铵鹌媪骜鳌钯呗钣鸨龅鹎贲锛荜哔滗铋筚跸苄缏笾骠飑飙镖镳鳔傧缤槟殡膑镔髌鬓禀饽钹鹁钸骖黪恻锸侪钗冁谄谶蒇忏婵骣觇禅镡伥苌怅阊鲳砗伧谌榇碜龀枨柽铖铛饬鸱铳俦帱雠刍绌蹰钏怆缍鹑辍龊鹚苁骢枞辏撺锉鹾哒鞑骀绐殚赕瘅箪谠砀裆焘镫籴诋谛绨觌镝巅钿癫铫鲷鲽铤铥岽鸫窦渎椟牍笃黩簖怼镦炖趸铎谔垩阏轭锇锷鹗颚颛鳄诶迩铒鸸鲕钫鲂绯镄鲱偾沣凫驸绂绋赙麸鲋鳆钆赅尴擀绀戆睾诰缟锆纥镉颍亘赓绠鲠诟缑觏诂毂钴锢鸪鹄鹘鸹掴诖掼鹳鳏犷匦刿妫桧鲑鳜衮绲鲧埚呙帼椁蝈铪阚绗颉灏颢诃阖蛎黉讧荭闳鲎浒鹕骅桦铧奂缳锾鲩鳇诙荟哕浍缋珲晖诨馄阍钬镬讦诘荠叽哜骥玑觊齑矶羁虿跻霁鲚鲫郏浃铗镓蛲谏缣戋戬睑鹣笕鲣鞯绛缰挢峤鹪鲛疖颌鲒卺荩馑缙赆觐刭泾迳弪胫靓阄鸠鹫讵屦榉飓钜锔窭龃锩镌隽谲珏皲剀垲忾恺铠锴龛闶钪铐骒缂轲钶锞颔龈铿喾郐哙脍狯髋诓诳邝圹纩贶匮蒉愦聩篑阃锟鲲蛴崃徕涞濑赉睐铼癞籁岚榄斓镧褴阆锒唠崂铑铹痨鳓诔缧俪郦坜苈莅蓠呖逦骊缡枥栎轹砺锂鹂疠粝跞雳鲡鳢蔹奁潋琏殓裢裣鲢魉缭钌鹩蔺廪檩辚躏绫棂蛏鲮浏骝绺镏鹨茏泷珑栊胧砻偻蒌喽嵝镂瘘耧蝼髅垆撸噜闾泸渌栌橹轳辂辘氇胪鸬鹭舻鲈脔娈栾鸾銮囵荦猡泺椤脶镙榈褛锊呒唛嬷杩劢缦镘颡鳗麽扪焖懑钔芈谧猕祢渑腼黾缈缪闵缗谟蓦馍殁镆钼铙讷铌鲵辇鲶茑袅陧蘖嗫颟蹑苎咛聍侬哝驽钕傩讴怄瓯蹒疱辔纰罴铍谝骈缥嫔钋镤镨蕲骐绮桤碛颀颃鳍佥荨悭骞缱椠钤嫱樯戗炝锖锵镪羟跄诮谯荞缲硗跷惬锲箧锓揿鲭茕蛱巯赇虮鳅诎岖阒觑鸲诠绻辁铨阕阙悫荛娆桡饪轫嵘蝾缛铷颦蚬飒毵糁缫啬铯穑铩鲨酾讪姗骟钐鳝垧殇觞厍滠畲诜谂渖谥埘莳弑轼贳铈鲥绶摅纾闩铄厮驷缌锶鸶薮馊飕锼谡稣谇荪狲唢睃闼铊鳎钛鲐昙钽锬顸傥饧铴镗韬铽缇鹈阗粜龆鲦恸钭钍抟饨箨鼍娲腽纨绾辋诿帏闱沩涠玮韪炜鲔阌莴龌邬庑怃妩骛鹉鹜饩阋玺觋硖苋莶藓岘猃娴鹇痫蚝籼跹芗饷骧缃飨哓潇骁绡枭箫亵撷绁缬陉荥馐鸺诩顼谖铉镟谑泶鳕埙浔鲟垭娅桠氩厣赝俨兖谳恹闫酽魇餍鼹炀轺鹞鳐靥谒邺晔烨诒呓峄饴怿驿缢轶贻钇镒镱瘗舣铟瘾茔莺萦蓥撄嘤滢潆璎鹦瘿颏罂镛莸铕鱿伛俣谀谕蓣嵛饫阈妪纡觎欤钰鹆鹬龉橼鸢鼋钺郓芸恽愠纭韫殒氲瓒趱錾驵赜啧帻箦谮缯谵诏钊谪辄鹧浈缜桢轸赈祯鸩诤峥钲铮筝骘栉栀轵轾贽鸷蛳絷踬踯觯锺纣绉伫槠铢啭馔颞骓缒诼镯谘缁辎赀眦锱龇鲻偬诹驺鲰镞缵躜鳟讠谫郄勐凼坂垅垴埯埝苘荬荮莜莼菰藁揸吒吣咔咝咴噘噼嚯幞岙嵴彷徼犸狍馀馇馓馕愣憷懔丬溆滟溷漤潴澹甯纟绔绱珉枧桊桉槔橥轱轷赍肷胨飚煳煅熘愍淼砜磙眍钚钷铘铞锃锍锎锏锘锝锪锫锿镅镎镢镥镩镲稆鹋鹛鹱疬疴痖癯裥襁耢颥螨麴鲅鲆鲇鲞鲴鲺鲼鳊鳋鳘鳙鞒鞴齄";
var 繁体字表 = "皚藹礙愛骯翺襖奧壩罷擺敗頒辦絆幫綁鎊謗剝飽寶報鮑輩貝鋇狽備憊繃筆畢斃幣閉邊編貶變辯辮標鱉別癟瀕濱賓擯餅並撥缽鉑駁蔔補財參蠶殘慚慘燦蒼艙倉滄廁側冊測層詫攙摻蟬饞讒纏鏟產闡顫場嘗長償腸廠暢鈔車徹塵沈陳襯撐稱懲誠騁癡遲馳恥齒熾沖蟲寵疇躊籌綢醜櫥廚鋤雛礎儲觸處傳瘡闖創錘純綽辭詞賜聰蔥囪從叢湊躥竄錯達帶貸擔單鄲撣膽憚誕彈當擋黨蕩檔搗島禱導盜燈鄧敵滌遞締顛點墊電澱雕釣調叠諜疊釘頂錠訂丟東動棟凍鬥犢獨讀賭鍍鍛斷緞兌隊對噸頓鈍奪墮鵝額訛惡餓兒爾餌貳發罰閥琺礬釩煩範販飯訪紡飛誹廢費紛墳奮憤糞豐楓鋒風瘋馮縫諷鳳膚輻撫輔賦復負訃婦縛該鈣蓋幹桿趕稈贛岡剛鋼綱崗臯鎬擱鴿閣鉻個給龔宮鞏貢鉤溝茍構購夠蠱顧剮掛關觀館慣貫廣規矽歸龜閨軌詭櫃貴劊輥滾鍋國過駭韓漢號閡鶴賀橫轟鴻紅後壺護滬戶嘩華畫劃話懷壞歡環還緩換喚瘓煥渙黃謊揮輝毀賄穢會燴匯諱誨繪葷渾夥獲貨禍擊機積饑跡譏雞績緝極輯級擠幾薊劑濟計記際繼紀夾莢頰賈鉀價駕殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗將漿蔣槳獎講醬膠澆驕嬌攪鉸矯僥腳餃繳絞轎較稭階節莖鯨驚經頸靜鏡徑痙競凈糾廄舊駒舉據鋸懼劇鵑絹傑潔結誡屆緊錦僅謹進晉燼盡勁荊覺決訣絕鈞軍駿開凱顆殼課墾懇摳庫褲誇塊儈寬礦曠況虧巋窺饋潰擴闊蠟臘萊來賴藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫瑯撈勞澇樂鐳壘類淚籬貍離裏鯉禮麗厲勵礫歷瀝隸倆聯蓮連鐮憐漣簾斂臉鏈戀煉練糧涼兩輛諒療遼鐐獵臨鄰鱗凜賃齡鈴淩靈嶺領餾劉龍聾嚨籠壟攏隴樓婁摟簍蘆盧顱廬爐擄鹵虜魯賂祿錄陸驢呂鋁侶屢縷慮濾綠巒攣孿灤亂掄輪倫侖淪綸論蘿羅邏鑼籮騾駱絡媽瑪碼螞馬罵嗎買麥賣邁脈瞞饅蠻滿謾貓錨鉚貿麽黴沒鎂門悶們錳夢瞇謎彌覓冪綿緬廟滅憫閩鳴銘謬謀畝吶鈉納難撓腦惱鬧餒內擬妳膩攆撚釀鳥聶嚙鑷鎳檸獰寧擰濘鈕紐膿濃農瘧諾歐鷗毆嘔漚盤龐拋賠噴鵬騙飄頻貧蘋憑評潑頗撲鋪樸譜棲淒臍齊騎豈啟氣棄訖牽扡釬鉛遷簽謙錢鉗潛淺譴塹槍嗆墻薔強搶鍬橋喬僑翹竅竊欽親寢輕氫傾頃請慶瓊窮趨區軀驅齲顴權勸卻鵲確讓饒擾繞熱韌認紉榮絨軟銳閏潤灑薩鰓賽三傘喪騷掃澀殺剎紗篩曬刪閃陜贍繕墑傷賞燒紹賒攝懾設紳審嬸腎滲聲繩勝聖師獅濕詩屍時蝕實識駛勢適釋飾視試壽獸樞輸書贖屬術樹豎數帥雙誰稅順說碩爍絲飼聳慫頌訟誦擻蘇訴肅雖隨綏歲孫損筍縮瑣鎖獺撻擡臺態攤貪癱灘壇譚談嘆湯燙濤絳討騰謄銻題體屜條貼鐵廳聽烴銅統頭禿圖塗團頹蛻脫鴕馱駝橢窪襪彎灣頑萬網韋違圍為濰維葦偉偽緯餵謂衛溫聞紋穩問甕撾蝸渦窩臥嗚鎢烏汙誣無蕪吳塢霧務誤錫犧襲習銑戲細蝦轄峽俠狹廈嚇鍁鮮纖鹹賢銜閑顯險現獻縣餡羨憲線廂鑲鄉詳響項蕭囂銷曉嘯蠍協挾攜脅諧寫瀉謝鋅釁興兇洶銹繡虛噓須許敘緒續軒懸選癬絢學勛詢尋馴訓訊遜壓鴉鴨啞亞訝閹煙鹽嚴巖顏閻艷厭硯彥諺驗鴦楊揚瘍陽癢養樣瑤搖堯遙窯謠藥爺頁業葉壹醫銥頤遺儀彜蟻藝億憶義詣議誼譯異繹蔭陰銀飲隱櫻嬰鷹應纓瑩螢營熒蠅贏穎喲擁傭癰踴詠湧優憂郵鈾猶遊誘於輿魚漁娛與嶼語籲禦獄譽預馭鴛淵轅園員圓緣遠願約躍鑰嶽粵悅閱雲鄖勻隕運蘊醞暈韻雜災載攢暫贊贓臟鑿棗竈責擇則澤賊贈紮劄軋鍘閘柵詐齋債氈盞斬輾嶄棧戰綻張漲帳賬脹趙蟄轍鍺這貞針偵診鎮陣掙睜猙爭幀癥鄭證織職執紙誌摯擲幟質滯鐘終種腫眾謅軸皺晝驟豬諸誅燭矚囑貯鑄築註駐專磚轉賺樁莊裝妝壯狀錐贅墜綴諄準著濁茲資漬蹤綜總縱鄒詛組鉆錒噯嬡璦曖靄諳銨鵪媼驁鰲鈀唄鈑鴇齙鵯賁錛蓽嗶潷鉍篳蹕芐緶籩驃颮飆鏢鑣鰾儐繽檳殯臏鑌髕鬢稟餑鈸鵓鈽驂黲惻鍤儕釵囅諂讖蕆懺嬋驏覘禪鐔倀萇悵閶鯧硨傖諶櫬磣齔棖檉鋮鐺飭鴟銃儔幬讎芻絀躕釧愴綞鶉輟齪鶿蓯驄樅輳攛銼鹺噠韃駘紿殫賧癉簞讜碭襠燾鐙糴詆諦綈覿鏑巔鈿癲銚鯛鰈鋌銩崠鶇竇瀆櫝牘篤黷籪懟鐓燉躉鐸諤堊閼軛鋨鍔鶚顎顓鱷誒邇鉺鴯鮞鈁魴緋鐨鯡僨灃鳧駙紱紼賻麩鮒鰒釓賅尷搟紺戇睪誥縞鋯紇鎘潁亙賡綆鯁詬緱覯詁轂鈷錮鴣鵠鶻鴰摑詿摜鸛鰥獷匭劌媯檜鮭鱖袞緄鯀堝咼幗槨蟈鉿闞絎頡灝顥訶闔蠣黌訌葒閎鱟滸鶘驊樺鏵奐繯鍰鯇鰉詼薈噦澮繢琿暉諢餛閽鈥鑊訐詰薺嘰嚌驥璣覬齏磯羈蠆躋霽鱭鯽郟浹鋏鎵蟯諫縑戔戩瞼鶼筧鰹韉絳韁撟嶠鷦鮫癤頜鮚巹藎饉縉贐覲剄涇逕弳脛靚鬮鳩鷲詎屨櫸颶鉅鋦窶齟錈鐫雋譎玨皸剴塏愾愷鎧鍇龕閌鈧銬騍緙軻鈳錁頷齦鏗嚳鄶噲膾獪髖誆誑鄺壙纊貺匱蕢憒聵簣閫錕鯤蠐崍徠淶瀨賚睞錸癩籟嵐欖斕鑭襤閬鋃嘮嶗銠鐒癆鰳誄縲儷酈壢藶蒞蘺嚦邐驪縭櫪櫟轢礪鋰鸝癘糲躒靂鱺鱧蘞奩瀲璉殮褳襝鰱魎繚釕鷯藺廩檁轔躪綾欞蟶鯪瀏騮綹鎦鷚蘢瀧瓏櫳朧礱僂蔞嘍嶁鏤瘺耬螻髏壚擼嚕閭瀘淥櫨櫓轤輅轆氌臚鸕鷺艫鱸臠孌欒鸞鑾圇犖玀濼欏腡鏍櫚褸鋝嘸嘜嬤榪勱縵鏝顙鰻麼捫燜懣鍆羋謐獼禰澠靦黽緲繆閔緡謨驀饃歿鏌鉬鐃訥鈮鯢輦鯰蔦裊隉蘗囁顢躡苧嚀聹儂噥駑釹儺謳慪甌蹣皰轡紕羆鈹諞駢縹嬪釙鏷鐠蘄騏綺榿磧頎頏鰭僉蕁慳騫繾槧鈐嬙檣戧熗錆鏘鏹羥蹌誚譙蕎繰磽蹺愜鍥篋鋟撳鯖煢蛺巰賕蟣鰍詘嶇闃覷鴝詮綣輇銓闋闕愨蕘嬈橈飪軔嶸蠑縟銣顰蜆颯毿糝繅嗇銫穡鎩鯊釃訕姍騸釤鱔坰殤觴厙灄畬詵諗瀋謚塒蒔弒軾貰鈰鰣綬攄紓閂鑠廝駟緦鍶鷥藪餿颼鎪謖穌誶蓀猻嗩脧闥鉈鰨鈦鮐曇鉭錟頇儻餳鐋鏜韜鋱緹鵜闐糶齠鰷慟鈄釷摶飩籜鼉媧膃紈綰輞諉幃闈溈潿瑋韙煒鮪閿萵齷鄔廡憮嫵騖鵡鶩餼鬩璽覡硤莧薟蘚峴獫嫻鷴癇蠔秈躚薌餉驤緗饗嘵瀟驍綃梟簫褻擷紲纈陘滎饈鵂詡頊諼鉉鏇謔澩鱈塤潯鱘埡婭椏氬厴贗儼兗讞懨閆釅魘饜鼴煬軺鷂鰩靨謁鄴曄燁詒囈嶧飴懌驛縊軼貽釔鎰鐿瘞艤銦癮塋鶯縈鎣攖嚶瀅瀠瓔鸚癭頦罌鏞蕕銪魷傴俁諛諭蕷崳飫閾嫗紆覦歟鈺鵒鷸齬櫞鳶黿鉞鄆蕓惲慍紜韞殞氳瓚趲鏨駔賾嘖幘簀譖繒譫詔釗謫輒鷓湞縝楨軫賑禎鴆諍崢鉦錚箏騭櫛梔軹輊贄鷙螄縶躓躑觶鍾紂縐佇櫧銖囀饌顳騅縋諑鐲諮緇輜貲眥錙齜鯔傯諏騶鯫鏃纘躦鱒訁譾郤猛氹阪壟堖垵墊檾蕒葤蓧蒓菇槁摣咤唚哢噝噅撅劈謔襆嶴脊仿僥獁麅餘餷饊饢楞怵懍爿漵灩混濫瀦淡寧糸絝緔瑉梘棬案橰櫫軲軤賫膁腖飈糊煆溜湣渺碸滾瞘鈈鉕鋣銱鋥鋶鐦鐧鍩鍀鍃錇鎄鎇鎿鐝鑥鑹鑔穭鶓鶥鸌癧屙瘂臒襇繈耮顬蟎麯鮁鮃鮎鯗鯝鯴鱝鯿鰠鰵鱅鞽韝齇";
var newNotice = new obsidian.Notice("欢迎使用 Obsidian！",1);
var 当前文件;
var 当前文件路径;
var 编辑模式;
var 聚焦编辑 = true;
var 所选文本 = "";
var 笔记正文 = "";
var 笔记全文;
var 处理文本 = "";
var 当前行文本 = "";
var 历史行文本 = "";
var 当前光标;
var 历史光标;
var 当前行号;
var 选至行首 = "";
var 选至行尾 = "";
var 末行行号;
var 末行文本 = "";
var 选至文首 = "";
var 选至文末 = "";
var 历史缩进 = "";
var 按上档键 = false;

var isShift = false;
var isIndent = true;
var isTool = false;
var isbt1Txt = false;
var isbt2Txt = false;
var isText = false;
var isGLS = false;
var isCTS = false;
var isXTS = false;
var isSCS = false;
var isXHS = false;
var isSB = false;
var isXB = false;
var isBgC = false;
var isCTxt = false;
var isGLS1 = false;
var isGLS2 = false;
var isGLS3 = false;

var isTHS = false; //涂黑
var isTCS = false; //涂彩
var isWKS = false; //挖空

function setAttributes(element, attributes) {
    for (let key in attributes) {
        element.setAttribute(key, attributes[key]);
    }
}

class MyPlugin extends obsidian.Plugin {
    constructor() {
        super(...arguments);
    };

    onload() {
        //console.log(t('loadThisPlugin'));
        this.loadSettings();
        this.footnoteStatusBar = this.addStatusBarItem();
        this.footnoteStatusBar.setText("");
        this.addCommand({
            id: 'set-mode',
            name: '切换模式',
            callback: () => this.切换模式()
        });
        this.addCommand({
            id: 'internal-link',
            name: '[[链接]]语法',
            callback: () => this.转换内部链接(),
            hotkeys: [{ modifiers: ["Alt"], key: "Z" } ]
        });
        //console.log('加载了'+this.settings.maxTry);
        this.addCommand({
            id: 'tag-text',
            name: '#标签 语法',
            callback: () => this.转换标签(),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "3" } ]
        });
        this.addCommand({
            id: 'tag-link-text',
            name: '标签双链互转',
            callback: () => this.标签双链互转(),
            hotkeys: [{ modifiers: ["Mod","Alt","Shift"], key: "3" } ]
        });

        /**/
        this.addCommand({
            id: 'all-links',
            name: '全局转换[[链接]]',
            callback: () => this.转换潜在链接()
        });
        this.addCommand({
            id: 'internal-link2',
            name: '[[链接|同名]]语法',
            callback: () => this.转换同义链接(),
            hotkeys: [{ modifiers: ["Alt"], key: "Q" } ]
        });
        this.addCommand({
            id: 'auto-text',
            name: '智能符号',
            callback: () => this.智能符号(),
            hotkeys: [{ modifiers: ["Alt"], key: ";"}]
        });

        this.addCommand({
            id: 'open-up',
            name: '查看同级上方文件',
            callback: () => this.切换文件列表(-1),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "U" } ]
        });

        this.addCommand({
            id: 'open-down',
            name: '查看同级下方文件',
            callback: () => this.切换文件列表(1),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "N" } ]
        });
        /*
        this.addCommand({
            id: 'open-RightWin',
            name: '开右窗口预览',
            callback: () => this.开右窗口预览(),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "L" } ]
        });
        */
        this.addCommand({
            id: 'leftWin-Up',
            name: '左窗向上滚动',
            callback: () => this.左窗向上滚动(),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "I" } ]
        });
        this.addCommand({
            id: 'leftWin-Down',
            name: '左窗向下滚动',
            callback: () => this.左窗向下滚动(),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "K" } ]
        });
        /**/

        this.addCommand({
            id: 'mouse-up',
            name: '游标上移',
            callback: () => this.游标上移(),
            hotkeys: [{ modifiers: ["Alt"], key: "I" } ]
        });
        this.addCommand({
            id: 'mouse-down',
            name: '游标下移',
            callback: () => this.游标下移(),
            hotkeys: [{ modifiers: ["Alt"], key: "K" } ]
        });
        this.addCommand({
            id: 'mouse-left',
            name: '游标左移',
            callback: () => this.游标左移(),
            hotkeys: [{ modifiers: ["Alt"], key: "J" } ]
        });
        this.addCommand({
            id: 'mouse-right',
            name: '游标右移',
            callback: () => this.游标右移(),
            hotkeys: [{ modifiers: ["Alt"], key: "L" } ]
        });
        this.addCommand({
            id: 'mouse-start',
            name: '游标移至行首',
            callback: () => this.游标移至行首(),
            hotkeys: [{ modifiers: ["Alt"], key: "U" } ]
        });
        this.addCommand({
            id: 'mouse-end',
            name: '游标移至行尾',
            callback: () => this.游标移至行尾(),
            hotkeys: [{ modifiers: ["Alt"], key: "O" } ]
        });
        this.addCommand({
            id: 'note-start',
            name: '游标移至文首',
            callback: () => this.游标移至文首()
        });
        this.addCommand({
            id: 'note-end',
            name: '游标移至文末',
            callback: () => this.游标移至文末()
        });

        this.addCommand({
            id: 'biaoti0-text',
            name: '取消标题',
            callback: () => this.标题语法(""),
            hotkeys: [{ modifiers: ["Mod"], key: "`" } ]
        });
        this.addCommand({
            id: 'biaoti1-text',
            name: 'H1标题',
            callback: () => this.标题语法("#")
            //hotkeys: [{ modifiers: ["Mod"], key: "1" } ]
        });
        this.addCommand({
            id: 'biaoti2-text',
            name: 'H2标题',
            callback: () => this.标题语法("##")
            //hotkeys: [{ modifiers: ["Mod"], key: "2" } ]
        }); 
        this.addCommand({
            id: 'biaoti3-text',
            name: 'H3标题',
            callback: () => this.标题语法("###")
            //hotkeys: [{ modifiers: ["Mod"], key: "3" } ]
        }); 
        this.addCommand({
            id: 'biaoti4-text',
            name: 'H4标题',
            callback: () => this.标题语法("####")
            //hotkeys: [{ modifiers: ["Mod"], key: "4" } ]
        }); 
        this.addCommand({
            id: 'biaoti5-text',
            name: 'H5标题',
            callback: () => this.标题语法("#####")
            //hotkeys: [{ modifiers: ["Mod"], key: "5" } ]
        }); 
        this.addCommand({
            id: 'biaoti6-text',
            name: 'H6标题',
            callback: () => this.标题语法("######")
            //hotkeys: [{ modifiers: ["Mod"], key: "6" } ]
        });
        this.addCommand({
            id: 'zeng-btexts',
            name: '调高标题级别',
            callback: () => this.调节标题级别(true)
        });
        this.addCommand({
            id: 'jian-btexts',
            name: '调低标题级别',
            callback: () => this.调节标题级别(false)
        });
        
        /**/
        this.addCommand({
            id: 'auto-texts',
            name: '自动设置标题',
            callback: () => this.自动设置标题()
        });
        this.addCommand({
            id: 'enlarge-texts',
            name: '大字号文本',
            callback: () => this.大字号文本()
        });
        
        this.addCommand({
            id: 'quit-format',
            name: '关闭格式刷',
            callback: () => {
                this.关闭格式刷();
                new obsidian.Notice("已关闭格式刷！");
            }
        }); 
        this.addCommand({
            id: 'cuti-format',
            name: '**粗体**格式刷',
            callback: () => this.粗体格式刷(),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "C" } ]
        }); 
        this.addCommand({
            id: 'cuti-text',
            name: '**粗体**',
            callback: () => this.转换粗体(),
            hotkeys: [{ modifiers: ["Alt"], key: "C" } ]
        }); 

        this.addCommand({
            id: 'gaoliang-format',
            name: '==高亮==格式刷',
            callback: () => this.高亮格式刷(),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "G" } ]
        });
        this.addCommand({
            id: 'gaoliang-text',
            name: '==高亮==',
            callback: () => this.转换高亮(),
            hotkeys: [{ modifiers: ["Alt"], key: "G" } ]
        });
		this.addCommand({
            id: 'gaoliang1-text',
            name: '*==多彩高亮1==*',
            callback: () => this.转换高亮1()
        });
		this.addCommand({
            id: 'gaoliang2-text',
            name: '**==多彩高亮2==**',
            callback: () => this.转换高亮2()
        });
		this.addCommand({
            id: 'gaoliang3-text',
            name: '***==多彩高亮3==***',
            callback: () => this.转换高亮3()
        });
		this.addCommand({
            id: 'tuhei-text',
            name: '==~~涂黑~~==',
            callback: () => this.转换涂黑()
        });
		this.addCommand({
            id: 'tucai-text',
            name: '*==~~涂彩~~==*',
            callback: () => this.转换涂彩()
        });
		this.addCommand({
            id: 'wakong-text',
            name: '*~~挖空~~*',
            callback: () => this.转换挖空()
        });
        /**/
        this.addCommand({
            id: 'xieti-format',
            name: '*斜体*格式刷',
            callback: () => this.斜体格式刷(),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "X" } ]
        });
        this.addCommand({
            id: 'xieti-text',
            name: '*斜体*',
            callback: () => this.转换斜体(),
            hotkeys: [{ modifiers: ["Alt"], key: "X" } ]
        });

        
        this.addCommand({
            id: 'shanchu-format',
            name: '~~删除线~~格式刷',
            callback: () => this.删除线格式刷(),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "S" } ]
        });
        this.addCommand({
            id: 'shanchu-text',
            name: '~~删除线~~',
            callback: () => this.转换删除线(),
            hotkeys: [{ modifiers: ["Alt"], key: "S" } ]
        });

        this.addCommand({
            id: 'xiahua-format',
            name: '_下划线_格式刷',
            callback: () => this.下划线格式刷(),
            hotkeys: [{ modifiers: ["Alt","Shift"], key: "H" } ]
        });
        this.addCommand({
            id: 'xiahua-text',
            name: '_下划线_',
            callback: () => this.转换下划线(),
            hotkeys: [{ modifiers: ["Alt"], key: "H" } ]
        });

        /* Obsidian自带此功能
        this.addCommand({
            id: 'zhuozhong-text',
            name: '`行内代码`',
            callback: () => this.转换行内代码()
        }); 
        */
        this.addCommand({
            id: 'add-daima',
            name: '```代码块```',
            callback: () => this.转换代码块(),
            hotkeys: [{ modifiers: ["Alt"], key: "D" } ]
        });
        this.addCommand({
            id: 'add-langxian',
            name: '~~~三浪线~~~',
            callback: () => this.转换三浪线()
        });
        this.addCommand({
            id: 'add-callout',
            name: '转换callout语法 ',
            callback: () => this.转换callout语法()
        });
        
        this.addCommand({
            id: 'internal-hyper',
            name: '[[]]转为[]()',
            callback: () => this.内链转为超链接()
        });
        this.addCommand({
            id: 'hyper-internal',
            name: '[]()转为[[]]',
            callback: () => this.超链接转为内链()
        });
        this.addCommand({
            id: 'clear-link',
            name: '去除超链接语法()',
            callback: () => this.去除超链接语法()
        });

        this.addCommand({
            id: 'common-text',
            name: '转换无语法文本',
            callback: () => this.转换无语法文本(),
            hotkeys: [{ modifiers: ["Mod","Alt"],key: "Z"}]
        });
        this.addCommand({
            id: 'copy-text',
            name: '获取无语法文本',
            callback: () => this.获取无语法文本(),
            hotkeys: [{ modifiers: ["Mod","Alt"],key: "C"}]
        });

        /*this.addCommand({
            id: 'format-up',
            name: '上标格式刷',
            callback: () => this.上标格式刷()
        });*/
        this.addCommand({
            id: 'add-up',
            name: '上标语法',
            callback: () => this.转换上标()
        });

        /*this.addCommand({
            id: 'format-ub',
            name: '下标格式刷',
            callback: () => this.下标格式刷()
        });*/
        this.addCommand({
            id: 'add-ub',
            name: '下标语法',
            callback: () => this.转换下标()
        });

        this.addCommand({
            id: 'text-Color1',
            name: '转换彩色文字1',
            callback: () => {
                this.settings.hColor = this.settings.hColor1;
                this.转换文字颜色();
            },
            hotkeys: [{ modifiers: ["Mod","Shift"], key: "1" } ]
        });
        this.addCommand({
            id: 'text-Color2',
            name: '转换彩色文字2',
            callback: () => {
                this.settings.hColor = this.settings.hColor2;
                this.转换文字颜色();
            },
            hotkeys: [{ modifiers: ["Mod","Shift"], key: "2" } ]
        });
        this.addCommand({
            id: 'text-Color3',
            name: '转换彩色文字3',
            callback: () => {
                this.settings.hColor = this.settings.hColor3;
                this.转换文字颜色();
            },
            hotkeys: [{ modifiers: ["Mod","Shift"], key: "3" } ]
        });
        this.addCommand({
            id: 'text-Color4',
            name: '转换彩色文字4',
            callback: () => {
                this.settings.hColor = this.settings.hColor4;
                this.转换文字颜色();
            },
            hotkeys: [{ modifiers: ["Mod","Shift"], key: "4" } ]
        });
        this.addCommand({
            id: 'text-Color5',
            name: '转换彩色文字5',
            callback: () => {
                this.settings.hColor = this.settings.hColor5;
                this.转换文字颜色();
            },
            hotkeys: [{ modifiers: ["Mod","Shift"], key: "5" } ]
        });
        this.addCommand({
            id: 'text-background1',
            name: '转换彩色背景1',
            callback: () => {
                this.settings.bColor = this.settings.bColor1;
                this.转换背景颜色();
            },
            hotkeys: [{ modifiers: ["Mod","Alt"], key: "1" } ]
        });
        this.addCommand({
            id: 'text-background2',
            name: '转换彩色背景2',
            callback: () => {
                this.settings.bColor = this.settings.bColor2;
                this.转换背景颜色();
            },
            hotkeys: [{ modifiers: ["Mod","Alt"], key: "2" } ]
        });
        this.addCommand({
            id: 'text-background3',
            name: '转换彩色背景3',
            callback: () => {
                this.settings.bColor = this.settings.bColor3;
                this.转换背景颜色();
            },
            hotkeys: [{ modifiers: ["Mod","Alt"], key: "3" } ]
        });
        this.addCommand({
            id: 'text-background4',
            name: '转换彩色背景4',
            callback: () => {
                this.settings.bColor = this.settings.bColor4;
                this.转换背景颜色();
            },
            hotkeys: [{ modifiers: ["Mod","Alt"], key: "4" } ]
        });
        this.addCommand({
            id: 'text-background5',
            name: '转换彩色背景5',
            callback: () => {
                this.settings.bColor = this.settings.bColor5;
                this.转换背景颜色();
            },
            hotkeys: [{ modifiers: ["Mod","Alt"], key: "5" } ]
        });

        this.addCommand({
            id: 'add-todo',
            name: '转换待办状态',
            callback: () => this.转换待办列表()
        });

        this.addCommand({
            id: 'sum-time',
            name: '合计任务用时',
            callback: () => this.合计任务用时()
        });
        this.addCommand({
            id: 'add-todoTime',
            name: '标记完成及时间',
            callback: () => this.标记完成及时间()
        });
        this.addCommand({
            id: 'y2w-list',
            name: '有转无序列表()',
            callback: () => this.有转无序列表()()
        });
        this.addCommand({
            id: 'w2y-list',
            name: '无转有序列表()',
            callback: () => this.无转有序列表()()
        });

        this.addCommand({
            id: 'add-tiankong',
            name: '填空{{c*::选文}}',
            callback: () => this.转换填空()
        });

        this.addCommand({
            id: 'list-mermaid',
            name: '列表转为图示',
            callback: () => this.列表转为图示() 
        });
        this.addCommand({
            id: 'file-path',
            name: '转换路径',
            callback: () => this.转换路径()
        });
        this.addCommand({
            id: 'text-line',
            name: '拆分多行',
            callback: () => this.拆分多行()
        });
        this.addCommand({
            id: 'jian-fan',
            name: '简体转繁',
            callback: () => this.简体转繁()
        });
        this.addCommand({
            id: 'fan-jian',
            name: '繁体转简',
            callback: () => this.繁体转简()
        }); 
        this.addCommand({
            id: 'yinhao-yinhao',
            name: '转换引号',
            callback: () => this.转换引号()
        });
        /*
        this.addCommand({
            id: 'add-kh1',
            name: '【选文】',
            callback: () => this.括选文本1()
        });
        this.addCommand({
            id: 'add-kh2',
            name: '（选文）',
            callback: () => this.括选文本2()
        });
        this.addCommand({
            id: 'add-kh3',
            name: '「选文」',
            callback: () => this.括选文本3()
        });
        this.addCommand({
            id: 'add-kh4',
            name: '《选文》',
            callback: () => this.括选文本4()
        });
        */
        /*
        this.addCommand({
            id: 'paste-html',
            name: '获取富文本()',
            callback: () => this.获取富文本()(),
            hotkeys: [{ modifiers: ["Mod","Shift","Alt"], key: "V" } ]
        });
        */
        this.addCommand({
            id: 'paste-text',
            name: '智能粘贴',
            callback: () => this.智能粘贴(),
            hotkeys: [{ modifiers: ["Mod","Alt"], key: "V" } ]
        });
        this.addCommand({
            id: 'paste-picText',
            name: '图文粘贴',
            callback: () => this.图文粘贴()
        });
        this.addCommand({
            id: 'jisuan-form',
            name: '计算所选结果',
            callback: () => this.计算所选结果(),
            hotkeys: [{ modifiers: [], key: "F9" } ]
        });
        this.addCommand({
            id: 'edit-intext',
            name: '修复外来文本',
            callback: () => this.修复外来文本()
        });
        this.addCommand({
            id: 'edit-biaodian',
            name: '修复错误标点',
            callback: () => this.修复错误标点()
        });
        this.addCommand({
            id: 'edit-yufa',
            name: '修复错误语法',
            callback: () => this.修复错误语法()
        });
        this.addCommand({
            id: 'edit-duanhang',
            name: '修复意外断行',
            callback: () => this.修复意外断行()
        });
        this.addCommand({
            id: 'search-text',
            name: '搜索当前文本',
            callback: () => this.搜索当前文本()
        });
        this.addCommand({
            id: 'delete-list',
            name: '删除当前段落',
            callback: () => this.删除当前段落()
        });
        this.addCommand({
            id: 'old-Cursor',
            name: '上次光标',
            callback: () => this.上次光标("")            
        });
        this.addCommand({
            id: 'parent-biaozhu',
            name: '光标向上跳转',
            callback: () => this.光标跳转("上")
        });
        this.addCommand({
            id: 'next-biaozhu',
            name: '光标向下跳转',
            callback: () => this.光标跳转("下")
        });
        
        this.addCommand({
            id: 'Selection-text',
            name: '选择当前整段',
            callback: () => this.选择当前整段()
        });
        this.addCommand({
            id: 'Selection-juzi',
            name: '选择当前整句',
            callback: () => this.选择当前整句()
        });
        this.addCommand({
            id: 'Selection-markdown',
            name: '选择当前语法',
            callback: () => this.选择当前语法()
        });
        this.addCommand({
            id: 'search-text',
            name: '获取搜索结果',
            callback: () => this.获取搜索结果()
        });
        this.addCommand({
            id: 'tiqu-text',
            name: '获取标注文本',
            callback: () => this.获取标注文本()
        });
        this.addCommand({
            id: 'copy-filePath',
            name: '获取相对路径',
            callback: () => this.获取相对路径()  
        });
        this.addCommand({
            id: 'modify-fileName',
            name: '指定当前文件名',
            callback: () => this.指定当前文件名()  
        });
        this.addCommand({
            id: 'iframe-URL',
            name: '嵌入当前网址页面',
            callback: () => this.嵌入当前网址页面() 
        });

        this.addCommand({
            id: 'sort-lines',
            name: '升序排列所选段落',
            callback: () => this.升序排列所选段落()
        });
        this.addCommand({
            id: 'reverse-lines',
            name: '降序排列所选段落',
            callback: () => this.降序排列所选段落()
        });
        /*
        this.addCommand({
            id: 'yinyong-list',
            name: '多行引用文本',
            callback: () => this.多行引用文本()
        });*/
        
        this.addCommand({
            id: 'jiaId-lines',
            name: '添加段落编号',
            callback: () => this.添加段落编号()
        });
        this.addCommand({
            id: 'jianId-lines',
            name: '去除段落编号',
            callback: () => this.去除段落编号()
        });

        this.addCommand({
            id: 'zhe-level-lines',
            name: '折叠同级标题',
            callback: () => this.折叠同级标题(),
            hotkeys: [{ modifiers: ["Mod","Shift","Alt"], key: "D" } ]
        });

        this.addCommand({
            id: 'promote-lines-level',
            name: '调高所有标题级别',
            callback: () => this.调高所有标题级别()
        });
        this.addCommand({
            id: 'Down-lines-level',
            name: '调低所有标题级别',
            callback: () => this.调低所有标题级别()
        });
        /*
        this.addCommand({
            id: 'promote-texts-level',
            name: '调高所选标题级别',
            callback: () => this.调高所选标题级别()
        });
        this.addCommand({
            id: 'Down-texts-level',
            name: '调低所选标题级别',
            callback: () => this.调低所选标题级别()
        });
        */

        this.addCommand({
            id: 'modify-link-showname',
            name: '修改内部链接的显示名称',
            callback: () => this.修改内部链接的显示名称(2),
            hotkeys: [{ modifiers: ["Alt"], key: "\\" } ]
        });
        this.addCommand({
            id: 'zhe-level2',
            name: '折叠二级标题',
            callback: () => this.折叠某级别标题(2),
            hotkeys: [{ modifiers: ["Shift"], key: "F2" } ]
        });
        this.addCommand({
            id: 'zhe-level3',
            name: '折叠三级标题',
            callback: () => this.折叠某级别标题(3),
            hotkeys: [{ modifiers: ["Shift"], key: "F3" } ]
        });
        this.addCommand({
            id: 'zhe-level4',
            name: '折叠四级标题',
            callback: () => this.折叠某级别标题(4),
            hotkeys: [{ modifiers: ["Shift"], key: "F4" } ]
        });
        this.addCommand({
            id: 'zhe-level5',
            name: '折叠五级标题',
            callback: () => this.折叠某级别标题(5),
            hotkeys: [{ modifiers: ["Shift"], key: "F5" } ]
        });
        this.addCommand({
            id: 'zhe-level6',
            name: '折叠六级标题',
            callback: () => this.折叠某级别标题(6),
            hotkeys: [{ modifiers: ["Shift"], key: "F6" } ]
        });

        this.addCommand({
            id: 'add-Line0',
            name: '插入有效空行',
            callback: () => this.插入有效空行(),
            hotkeys: [{ modifiers: ["Mod","Shift","Alt"], key: "Enter" } ]
        });
        this.addCommand({
            id: 'space-lines',
            name: '空格转为空行',
            callback: () => this.空格转为空行()
        });
        this.addCommand({
            id: 'add-lines',
            name: '批量插入空行',
            callback: () => this.批量插入空行(),
            hotkeys: [{ modifiers: ["Mod","Shift"], key: "L" } ]    
        });
        this.addCommand({
            id: 'add-line1',
            name: '上方插入空行',
            callback: () => this.上方插入空行(), 
        });
        this.addCommand({
            id: 'add-line2',
            name: '下方插入空行',
            callback: () => this.下方插入空行()
        });
        this.addCommand({
            id: 'del-lines',
            name: '批量去除空行',
            callback: () => this.批量去除空行(),
            hotkeys: [{ modifiers: ["Mod","Alt"], key: "l" } ]
        });
        this.addCommand({
            id: 'add-twoSpace',
            name: '全文行首缩进/取消缩进',
            callback: () => this.全文首行缩进()
        });
        this.addCommand({
            id: 'this-twoSpace',
            name: '当前行缩进/取消缩进两字符',
            callback: () => this.当前行缩进()
        });
        this.addCommand({
            id: 'add-space2',
            name: '行首添加空格',
            callback: () => this.行首添加空格()
        });
        this.addCommand({
            id: 'del-space1',
            name: '去除行首空格',
            callback: () => this.去除行首空格()
        });
        this.addCommand({
            id: 'add-space1',
            name: '末尾追加空格',
            callback: () => this.末尾追加空格()
        });
        this.addCommand({
            id: 'del-space2',
            name: '去除末尾空格',
            callback: () => this.去除末尾空格()
        });
        this.addCommand({
            id: 'add-allSpspace',
            name: '添加间隔空格',
            callback: () => this.添加间隔空格()
        });
        this.addCommand({
            id: 'del-allSpspace',
            name: '去除所有空格',
            callback: () => this.去除所有空格()
        });
        this.addCommand({
            id: 'del-allZhushi',
            name: '去除所有注释',
            callback: () => this.去除所有注释()
        });


        this.addSettingTab(new editSettingsTab(this.app, this));
        this.app.workspace.onLayoutReady(() => {
            setTimeout(() => {
                this.html语法格式刷();
                this.实用命令菜单();
            });
        });
        
        
        /*
        this.registerEvent(this.app.workspace.on('editor-change', function (file) {
            if (file) {
                
            };
        }));
        */

        document.addEventListener('mouseup', (e) => {
            历史光标=当前光标;
            this.获取编辑器信息 ();
            if(聚焦编辑){
                历史行文本 = 当前行文本;
                if(所选文本 == ""){
                    return
                }else{
                    if(isText){
                        this.转换无语法文本();
                    }else if(isbt1Txt){
                        this.标题语法("#")
                    }else if(isbt2Txt){
                        this.标题语法("##")
                    }else if(isCTxt){
                        this.转换文字颜色();
                    }else if(isBgC){
                        this.转换背景颜色();
                    }else if(isGLS){
                        this.转换高亮();
                    }else if(isGLS1){
                        this.转换高亮1();
					}else if(isGLS2){
                        this.转换高亮2();
                    }else if(isGLS3){
                        this.转换高亮3();
					}else if(isCTS){
                        this.转换粗体();
                    }else if(isXTS){
                        this.转换斜体();
                    }else if(isSCS){
                        this.转换删除线();
                    }else if(isXHS){
                        this.转换下划线();
                    }else if(isSB){
                        this.转换上标();
                    }else if(isXB){
                        this.转换下标();
                    }else if(isTHS){
                        this.转换涂黑();
                    }else if(isTCS){
                        this.转换涂彩();
                    }else if(isWKS){
                        this.转换挖空();
                    }
                    //else if(isShift){
                        //newNotice.hide();
                        // newNotice = new obsidian.Notice(所选文本);
                    //};
                }
                 
            }else if(isText||isCTxt ||isBgC ||isCTS || isGLS || isGLS1 ||isGLS2 ||isGLS3 ||isSB || isSCS || isXB || isXHS || isXTS || isTHS ||isTCS||isWKS || isbt2Txt || isbt1Txt){
                this.关闭格式刷();
                newNotice = new obsidian.Notice("已关闭格式刷！");
            }
        });
        
        document.addEventListener('keydown',(e) =>{
            this.获取编辑器信息 ();
            if(聚焦编辑){
                /*
                if(e.key == "Tab" && this.settings.isTab){
                    选至行首 = 选至行首.replace(/^(    |\t)/m,"");    //消除此次缩进效果
                    if(/^\s*([\-\*]+|`+|\|[^\|]|[\-\+]\s|\d+\.\s)/.test(选至行首)){
                        //当前为分隔行、代码块、列表行或表格行，则不在行内插入制表符
                    }else{
                        笔记全文.replaceRange(选至行首+"‌　　", {line:当前行号,ch:0}, 当前光标); //后补两个全角空格
                    }
                };

                if(e.key =="Tab" && isShift && this.settings.isTab){
                    笔记全文.replaceRange(选至行首.replace(/[‌　]+$/,""), {line:当前行号,ch:0}, 当前光标); //去除行首缩进
                    编辑模式.setCursor({line:当前行号,ch:0});
                }*/

                if(e.key =="Shift"){
                    isShift =true;
                }
                if(e.key == "Esc" || e.key == "Escape"){
                    if(isText||isCTxt ||isBgC ||isCTS || isGLS || isGLS1 ||isGLS2 ||isGLS3 ||isSB || isSCS || isXB || isXHS || isXTS || isTHS ||isTCS||isWKS || isbt2Txt || isbt1Txt){
                        this.关闭格式刷();
                        newNotice = new obsidian.Notice("已关闭格式刷！");
                    }
                }
            };
        });
        
        document.addEventListener('keyup',(e) =>{
            this.获取编辑器信息 ();
            if(聚焦编辑){
                
                if(e.key =="Shift"){
                    isShift = false;
                }
            };            
        });

        /** 以下四个侦听函数备用 */
        
        this.registerEvent(this.app.workspace.on('file-open', (file) => {
            if (file && file.path) {
                document.title = this.app.vault.adapter.basePath + "\\" +file.path.replace(/\//g,"\\");
                const files = this.app.vault.getFiles();
                const FilePaths = files.map((file) => {
                    return file.path;
                });
                let attachDir = this.app.vault.getConfig("attachmentFolderPath");
                //console.log("附件目录\n"+attachDir);
                
                当前文件 = file;
                当前文件路径 = file.path;
                
                if(this.settings.version != 当前版本){
                    const noticeContent = createFragment();
                    noticeContent.createEl("p").innerHTML = "<b>欢迎使用增强编辑插件！</b>";
                    noticeContent.createEl("p").innerHTML = 功能更新;
                    noticeContent.createEl("button").innerHTML = 宣传页面;
                    noticeContent.createEl("p").innerHTML = '点击此处 可关闭提示窗口......';
                    new obsidian.Notice(noticeContent, 0);
                    this.settings.version = 当前版本;
                    this.saveSettings();
                };
                //this.显示写作进度();                
            }
        }));

        /*
        this.registerEvent(this.app.vault.on('delete', (file) => {
            if (file && file.path) {
                this.saveSettings();
            }
        }));

        this.registerEvent(this.app.vault.on('rename', (file, oldPath) => {
            if (file && file.path) {
                this.saveSettings();
            }
        }));
        
        this.registerCodeMirror((cm) => {
            let cmEditor = cm;
            let currentExtraKeys = cmEditor.getOption('extraKeys');
            let moreKeys = {
                'Enter': (cm) => {
                    编辑模式 = this.获取编辑模式 ();
                    当前光标 = 编辑模式.getCursor();
                }
            };            
        });
        */
    };



    显示写作进度(){
        this.获取编辑器信息 ();
        if(this.settings.isShowNum){
            let maxTry = this.settings.maxTry;
            let 进度 = Math.round(笔记正文.length/maxTry*100)+"% → "+maxTry +"字";
            //console.log(maxTry+" "+进度);
            this.footnoteStatusBar.setText(进度);
        }
    }

    实用命令菜单() {
        this.statusBarIcon = this.addStatusBarItem();
        this.statusBarIcon.addClass("Enhanced-statusbar-button");
        obsidian.addIcon("md语法图标", 全局命令图标);
        obsidian.setIcon(this.statusBarIcon, "md语法图标");
        this.registerDomEvent(this.statusBarIcon, "click", (e) => {
            const activePane = this.app.workspace.activeLeaf;
            let 当前模式 =activePane.getViewState();
            if (当前模式.type === "empty") {
                return;
              }
		    if(当前模式.state.mode == "preview"){
                this.app.commands.executeCommandById("markdown:toggle-preview");
            };

            const statusBarRect2 = this.statusBarIcon.parentElement.getBoundingClientRect();
            const statusBarIconRect2 = this.statusBarIcon.getBoundingClientRect();
            const menu = new obsidian.Menu(this.app);

            menu.addItem((item) => {
                item.setTitle("设置插件");
                item.setIcon("gear");
                item.onClick(() =>{
                    this.app.setting.open();
                    this.app.setting.openTabById("Enhanced-editing");
                });
            });
            menu.addItem((item) => {
                item.setTitle("打开插件文件夹");
                item.setIcon("gear");
                item.onClick(() =>{
                    this.app.openWithDefaultApp(this.app.vault.configDir + "/plugins");
                    console.log(this.app.vault.configDir + "/plugins");
                });
            });
            menu.addItem((item) => {
                item.setTitle("设置快捷键");
                item.setIcon("gear");
                item.onClick(() =>{
                    this.app.setting.open();
                    var e = this.app.setting.openTabById("hotkeys");
                    e.setQuery("增强编辑");
                });
            });
            menu.addItem((item) => {
                item.setTitle("去除所有空格");
                item.setIcon("bracket-glyph");
                item.onClick(() =>this.去除所有空格());
            });
            menu.addItem((item) => {
                item.setTitle("添加间隔空格");
                item.setIcon("bracket-glyph");
                item.onClick(() =>this.添加间隔空格());
            });
            menu.addItem((item) => {
                item.setTitle("全文首行缩进");
                item.setIcon("indent-glyph");
                item.onClick(() =>this.全文首行缩进());
            });
            menu.addItem((item) => {
                item.setTitle("批量去除空行");
                item.setIcon("expand-vertically");
                item.onClick(() =>this.批量去除空行());
            });
            menu.addItem((item) => {
                item.setTitle("批量插入空行");
                item.setIcon("expand-vertically");
                item.onClick(() =>this.批量插入空行());
            });
            menu.addItem((item) => {
                item.setTitle("折叠同级标题");
                item.setIcon("double-up-arrow-glyph");
                item.onClick(() =>this.折叠同级标题());
            });
            menu.addItem((item) => {
                item.setTitle("提升多个标题等级");
                item.setIcon("double-up-arrow-glyph");
                item.onClick(() =>this.调高所有标题级别());
            });
            menu.addItem((item) => {
                item.setTitle("下调多个标题等级");
                item.setIcon("double-down-arrow-glyph");
                item.onClick(() =>this.调低所有标题级别());
            });
            menu.addItem((item) => {
                item.setTitle("嵌入当前网址");
                item.setIcon("link");
                item.onClick(() =>this.嵌入当前网址页面());
            });
            menu.addItem((item) => {
                item.setTitle("获取搜索结果");
                item.setIcon("link");
                item.onClick(() =>this.获取搜索结果());
            });
            menu.addItem((item) => {
                item.setTitle("获取当前字数");
                item.setIcon("info");
                item.onClick(() =>this.获取当前字数());
            });
            menu.addItem((item) => {
                item.setTitle("自动设置标题");
                item.setIcon("heading-glyph");
                item.onClick(() =>this.自动设置标题());
            });
            menu.addItem((item) => {
                item.setTitle("列表转为图示");
                item.setIcon("dot-network");
                item.onClick(() =>this.列表转为图示());
            });
            menu.addItem((item) => {
                item.setTitle("修复外来文本");
                item.setIcon("indent-glyph");
                item.onClick(() =>this.修复外来文本());
            });
            menu.addItem((item) => {
                item.setTitle("修复错误语法");
                item.setIcon("check-small");
                item.onClick(() =>this.修复错误语法());
            });
            menu.addItem((item) => {
                item.setTitle("修复意外断行");
                item.setIcon("indent-glyph");
                item.onClick(() =>this.修复意外断行());
            });
            menu.addItem((item) => {
                item.setTitle("转换潜在链接");
                item.setIcon("broken-link");
                item.onClick(() =>this.转换潜在链接());
            });
            menu.showAtPosition({
                x: statusBarIconRect2.right + 5,
                y: statusBarRect2.top - 10,
            });
        });
    }
 

    html语法格式刷() {
        this.statusBarIcon = this.addStatusBarItem();
       
        this.statusBarIcon.addClass("ewt-statusbar-button");
        obsidian.addIcon("格式刷图标", 格式刷图标);
        obsidian.addIcon("普通格式刷", 普通格式刷);
        obsidian.setIcon(this.statusBarIcon, "格式刷图标");

        this.registerDomEvent(this.statusBarIcon, "click", (e) => {
            const 格式刷1 ='<svg t="1650117667147" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="12959" width="120" height="120"><path d="M409.856 331.9296l103.1936-103.168 307.712 307.712-103.168 103.168z" fill="#777677" p-id="12960"></path><path d="M384 358.4s-153.6 128-256 99.84c23.04 38.4 53.76 76.8 51.2 79.36 79.36 17.92 204.8-51.2 204.8-51.2l25.6 25.6s-133.12 102.4-204.8 76.8c66.56 99.84 212.48 225.28 256 256 97.28 0 230.4-179.2 230.4-179.2L384 358.4z" fill="'+this.settings.bColor1+'" p-id="12961"></path><path d="M641.3568 306.9952l153.856-153.856 103.1936 103.168-153.856 153.856z" fill="#777677" p-id="12962"></path></svg>';
            const 格式刷2 ='<svg t="1650117667147" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="12959" width="120" height="120"><path d="M409.856 331.9296l103.1936-103.168 307.712 307.712-103.168 103.168z" fill="#777677" p-id="12960"></path><path d="M384 358.4s-153.6 128-256 99.84c23.04 38.4 53.76 76.8 51.2 79.36 79.36 17.92 204.8-51.2 204.8-51.2l25.6 25.6s-133.12 102.4-204.8 76.8c66.56 99.84 212.48 225.28 256 256 97.28 0 230.4-179.2 230.4-179.2L384 358.4z" fill="'+this.settings.bColor2+'" p-id="12961"></path><path d="M641.3568 306.9952l153.856-153.856 103.1936 103.168-153.856 153.856z" fill="#777677" p-id="12962"></path></svg>';
            const 格式刷3 ='<svg t="1650117667147" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="12959" width="120" height="120"><path d="M409.856 331.9296l103.1936-103.168 307.712 307.712-103.168 103.168z" fill="#777677" p-id="12960"></path><path d="M384 358.4s-153.6 128-256 99.84c23.04 38.4 53.76 76.8 51.2 79.36 79.36 17.92 204.8-51.2 204.8-51.2l25.6 25.6s-133.12 102.4-204.8 76.8c66.56 99.84 212.48 225.28 256 256 97.28 0 230.4-179.2 230.4-179.2L384 358.4z" fill="'+this.settings.bColor3+'" p-id="12961"></path><path d="M641.3568 306.9952l153.856-153.856 103.1936 103.168-153.856 153.856z" fill="#777677" p-id="12962"></path></svg>';
            const 格式刷4 ='<svg t="1650117667147" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="12959" width="120" height="120"><path d="M409.856 331.9296l103.1936-103.168 307.712 307.712-103.168 103.168z" fill="#777677" p-id="12960"></path><path d="M384 358.4s-153.6 128-256 99.84c23.04 38.4 53.76 76.8 51.2 79.36 79.36 17.92 204.8-51.2 204.8-51.2l25.6 25.6s-133.12 102.4-204.8 76.8c66.56 99.84 212.48 225.28 256 256 97.28 0 230.4-179.2 230.4-179.2L384 358.4z" fill="'+this.settings.bColor4+'" p-id="12961"></path><path d="M641.3568 306.9952l153.856-153.856 103.1936 103.168-153.856 153.856z" fill="#777677" p-id="12962"></path></svg>';
            const 格式刷5 ='<svg t="1650117667147" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="12959" width="120" height="120"><path d="M409.856 331.9296l103.1936-103.168 307.712 307.712-103.168 103.168z" fill="#777677" p-id="12960"></path><path d="M384 358.4s-153.6 128-256 99.84c23.04 38.4 53.76 76.8 51.2 79.36 79.36 17.92 204.8-51.2 204.8-51.2l25.6 25.6s-133.12 102.4-204.8 76.8c66.56 99.84 212.48 225.28 256 256 97.28 0 230.4-179.2 230.4-179.2L384 358.4z" fill="'+this.settings.bColor5+'" p-id="12961"></path><path d="M641.3568 306.9952l153.856-153.856 103.1936 103.168-153.856 153.856z" fill="#777677" p-id="12962"></path></svg>';
            
            const 文本刷1 ='<svg t="1650187348745" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="41653" ><path d="M126.656 832c0-35.2 28.8-64 64-64H832c35.2 0 64 28.8 64 64s-28.8 64-64 64H190.656c-35.2 0-64-28.8-64-64z" fill="'+this.settings.hColor1+'" p-id="41654"></path><path d="M587.2 151.936C582.208 138.752 567.04 128 553.472 128H470.528a39.232 39.232 0 0 0-33.792 23.936l-201.024 528.128c-4.992 13.184 2.432 23.936 16.512 23.936h85.76c14.08 0 29.632-10.816 34.496-24l29.632-80A39.616 39.616 0 0 1 436.608 576h150.784c14.08 0 29.632 10.816 34.496 24l29.632 80a39.616 39.616 0 0 0 34.496 24h85.76c14.08 0 21.504-10.752 16.512-23.936L587.2 151.936zM502.912 338.048c4.992-13.184 13.184-13.184 18.176 0l32.768 86.016c4.992 13.184-2.432 23.936-16.512 23.936h-50.688c-14.08 0-21.504-10.752-16.512-23.936l32.768-86.016z" fill="#A5A6A7" p-id="41655"></path></svg>';
            const 文本刷2 ='<svg t="1650187348745" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="41653" ><path d="M126.656 832c0-35.2 28.8-64 64-64H832c35.2 0 64 28.8 64 64s-28.8 64-64 64H190.656c-35.2 0-64-28.8-64-64z" fill="'+this.settings.hColor2+'" p-id="41654"></path><path d="M587.2 151.936C582.208 138.752 567.04 128 553.472 128H470.528a39.232 39.232 0 0 0-33.792 23.936l-201.024 528.128c-4.992 13.184 2.432 23.936 16.512 23.936h85.76c14.08 0 29.632-10.816 34.496-24l29.632-80A39.616 39.616 0 0 1 436.608 576h150.784c14.08 0 29.632 10.816 34.496 24l29.632 80a39.616 39.616 0 0 0 34.496 24h85.76c14.08 0 21.504-10.752 16.512-23.936L587.2 151.936zM502.912 338.048c4.992-13.184 13.184-13.184 18.176 0l32.768 86.016c4.992 13.184-2.432 23.936-16.512 23.936h-50.688c-14.08 0-21.504-10.752-16.512-23.936l32.768-86.016z" fill="#A5A6A7" p-id="41655"></path></svg>';
            const 文本刷3 ='<svg t="1650187348745" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="41653" ><path d="M126.656 832c0-35.2 28.8-64 64-64H832c35.2 0 64 28.8 64 64s-28.8 64-64 64H190.656c-35.2 0-64-28.8-64-64z" fill="'+this.settings.hColor3+'" p-id="41654"></path><path d="M587.2 151.936C582.208 138.752 567.04 128 553.472 128H470.528a39.232 39.232 0 0 0-33.792 23.936l-201.024 528.128c-4.992 13.184 2.432 23.936 16.512 23.936h85.76c14.08 0 29.632-10.816 34.496-24l29.632-80A39.616 39.616 0 0 1 436.608 576h150.784c14.08 0 29.632 10.816 34.496 24l29.632 80a39.616 39.616 0 0 0 34.496 24h85.76c14.08 0 21.504-10.752 16.512-23.936L587.2 151.936zM502.912 338.048c4.992-13.184 13.184-13.184 18.176 0l32.768 86.016c4.992 13.184-2.432 23.936-16.512 23.936h-50.688c-14.08 0-21.504-10.752-16.512-23.936l32.768-86.016z" fill="#A5A6A7" p-id="41655"></path></svg>';
            const 文本刷4 ='<svg t="1650187348745" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="41653" ><path d="M126.656 832c0-35.2 28.8-64 64-64H832c35.2 0 64 28.8 64 64s-28.8 64-64 64H190.656c-35.2 0-64-28.8-64-64z" fill="'+this.settings.hColor4+'" p-id="41654"></path><path d="M587.2 151.936C582.208 138.752 567.04 128 553.472 128H470.528a39.232 39.232 0 0 0-33.792 23.936l-201.024 528.128c-4.992 13.184 2.432 23.936 16.512 23.936h85.76c14.08 0 29.632-10.816 34.496-24l29.632-80A39.616 39.616 0 0 1 436.608 576h150.784c14.08 0 29.632 10.816 34.496 24l29.632 80a39.616 39.616 0 0 0 34.496 24h85.76c14.08 0 21.504-10.752 16.512-23.936L587.2 151.936zM502.912 338.048c4.992-13.184 13.184-13.184 18.176 0l32.768 86.016c4.992 13.184-2.432 23.936-16.512 23.936h-50.688c-14.08 0-21.504-10.752-16.512-23.936l32.768-86.016z" fill="#A5A6A7" p-id="41655"></path></svg>';
            const 文本刷5 ='<svg t="1650187348745" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="41653" ><path d="M126.656 832c0-35.2 28.8-64 64-64H832c35.2 0 64 28.8 64 64s-28.8 64-64 64H190.656c-35.2 0-64-28.8-64-64z" fill="'+this.settings.hColor5+'" p-id="41654"></path><path d="M587.2 151.936C582.208 138.752 567.04 128 553.472 128H470.528a39.232 39.232 0 0 0-33.792 23.936l-201.024 528.128c-4.992 13.184 2.432 23.936 16.512 23.936h85.76c14.08 0 29.632-10.816 34.496-24l29.632-80A39.616 39.616 0 0 1 436.608 576h150.784c14.08 0 29.632 10.816 34.496 24l29.632 80a39.616 39.616 0 0 0 34.496 24h85.76c14.08 0 21.504-10.752 16.512-23.936L587.2 151.936zM502.912 338.048c4.992-13.184 13.184-13.184 18.176 0l32.768 86.016c4.992 13.184-2.432 23.936-16.512 23.936h-50.688c-14.08 0-21.504-10.752-16.512-23.936l32.768-86.016z" fill="#A5A6A7" p-id="41655"></path></svg>';


            obsidian.addIcon("格式刷1", 格式刷1);
            obsidian.addIcon("格式刷2", 格式刷2);
            obsidian.addIcon("格式刷3", 格式刷3);
            obsidian.addIcon("格式刷4", 格式刷4);
            obsidian.addIcon("格式刷5", 格式刷5);
            obsidian.addIcon("文本刷1", 文本刷1);
            obsidian.addIcon("文本刷2", 文本刷2);
            obsidian.addIcon("文本刷3", 文本刷3);
            obsidian.addIcon("文本刷4", 文本刷4); 
            obsidian.addIcon("文本刷5", 文本刷5);
            const activePane = this.app.workspace.activeLeaf;
            let 当前模式 =activePane.getViewState();
            if (当前模式.type === "empty") {
                return;
              }
		    if(当前模式.state.mode == "preview"){
            this.app.commands.executeCommandById("markdown:toggle-preview");
               };
            if(isCTxt ||isBgC ||isCTS || isGLS ||isGLS1 ||isGLS2 ||isGLS3 || isSB || isSCS || isXB || isXHS || isXTS|| isTHS ||isTCS||isWKS){
                this.关闭格式刷();
                new obsidian.Notice("已关闭格式刷！");
            }

            const statusBarRect1 = this.statusBarIcon.parentElement.getBoundingClientRect();
            const statusBarIconRect1 = this.statusBarIcon.getBoundingClientRect();
            const menu = new obsidian.Menu(this.app);
            
            const menuDom = menu.dom;
            menuDom.addClass("Enhanced-editing-menu");

            menu.addItem((item) => {
                item.setTitle("设置插件");
                item.setIcon("gear");
                item.setSection("settings");
                item.onClick(() =>{
                    this.app.setting.open();
                    this.app.setting.openTabById("Enhanced-editing");
                });
            });
            menu.addItem((item) => {
                item.setTitle("设置快捷键");
                item.setIcon("gear");
                item.setSection("settings");
                item.onClick(() =>{
                    this.app.setting.open();
                    var e = this.app.setting.openTabById("hotkeys");
                    e.setQuery("增强编辑");
                });
            });
            /*
            menu.addItem((item) => {
                item.setTitle("打开增强命令");
                item.setIcon("gear");
                item.onClick(() =>{
                    //this.app.setting.open();
                    this.app.commands.executeCommandById('command-palette:open');
                });
            });
            */
            menu.addItem((item) => {
                item.setTitle("刷为一级标题");
                item.setIcon("普通格式刷");
                item.setSection("biaoti");
                item.onClick(() =>{
                    this.关闭格式刷();
                    isbt1Txt = true;
                    new obsidian.Notice("一级标题格式刷 已打开！")
                });
            });
            menu.addItem((item) => {
                item.setTitle("刷为二级标题");
                item.setIcon("普通格式刷");
                item.setSection("biaoti");
                item.onClick(() =>{
                    this.关闭格式刷();
                    isbt2Txt = true;
                    new obsidian.Notice("二级标题格式刷 已打开！")
                });
            });
            
            menu.addItem((item) => {
                item.setTitle("**粗体**");
                item.setIcon("普通格式刷");
                item.setSection("format");
                item.onClick(() =>this.粗体格式刷());
            });
            menu.addItem((item) => {
                item.setTitle("==高亮==");
                item.setIcon("普通格式刷");
                item.setSection("format");
                item.onClick(() =>this.高亮格式刷());
            });
            menu.addItem((item) => {
                item.setTitle("~~删除线~~");
                item.setIcon("普通格式刷");
                item.setSection("format");
                item.onClick(() =>this.删除线格式刷());
            });
            menu.addItem((item) => {
                item.setTitle(" *斜体* ");
                item.setIcon("普通格式刷");
                item.setSection("format");
                item.onClick(() =>this.斜体格式刷());
            });
            menu.addItem((item) => {
                item.setTitle(" 上标⁺⁻ⁿ ");
                item.setIcon("普通格式刷");
                item.setSection("format");
                item.onClick(() =>this.上标格式刷());
            });
            menu.addItem((item) => {
                item.setTitle(" 下标₁₂₃ ");
                item.setIcon("普通格式刷");
                item.setSection("format");
                item.onClick(() =>this.下标格式刷());
            });
            menu.addItem((item) => {
                item.setTitle("纯文本");
                item.setIcon("普通格式刷");
                item.setSection("format");
                item.onClick(() =>{
                    this.关闭格式刷();
                    isText = true;
                    new obsidian.Notice("纯文本格式刷 已打开！")
                });
            });

            /*
            menu.addItem((item) => {
                item.setTitle("关闭 格式刷");
                item.setIcon("cross");
                item.onClick(() =>{
                    this.关闭格式刷();
                    new obsidian.Notice("已关闭格式刷！");
                });
            });*/

            menu.addItem((item) => {
                item.setTitle("文本颜色1");
                //item.setIcon("文本刷1");
                item.setIcon("普通格式刷");
                item.setSection("fontcolor");
                item.onClick(() => this.彩字格式刷(this.settings.hColor1,'文本颜色1'));
            });

            menu.addItem((item) => {
                item.setTitle("文本颜色2");
                //item.setIcon("文本刷2");
                item.setIcon("普通格式刷");
                item.setSection("fontcolor");
                item.onClick(() => this.彩字格式刷(this.settings.hColor2,'文本颜色2'));
            });

            menu.addItem((item) => {
                item.setTitle("文本颜色3");
                //item.setIcon("文本刷3");
                item.setIcon("普通格式刷");
                item.setSection("fontcolor");
                item.onClick(() => this.彩字格式刷(this.settings.hColor3,'文本颜色3'));
            });
            menu.addItem((item) => {
                item.setTitle("文本颜色4");
                //item.setIcon("文本刷4");
                item.setIcon("普通格式刷");
                item.setSection("fontcolor");
                item.onClick(() => this.彩字格式刷(this.settings.hColor4,'文本颜色4'));
            });
            menu.addItem((item) => {
                item.setTitle("文本颜色5");
                //item.setIcon("文本刷5");
                item.setIcon("普通格式刷");
                item.setSection("fontcolor");
                item.onClick(() => this.彩字格式刷(this.settings.hColor5,'文本颜色5'));
            });

            menu.addItem((item) => {
                item.setTitle("荧光笔1");
                //item.setIcon("格式刷1");
                item.setIcon("普通格式刷");
                item.setSection("highlight_html");
                item.onClick(() => this.彩底格式刷(this.settings.bColor1,'荧光笔1'));
            });
            menu.addItem((item) => {
                item.setTitle("荧光笔2");
                //item.setIcon("格式刷2");
                item.setIcon("普通格式刷");
                item.setSection("highlight_html");
                item.onClick(() => this.彩底格式刷(this.settings.bColor2,'荧光笔2'));
            });
            menu.addItem((item) => {
                item.setTitle("荧光笔3");
                //item.setIcon("格式刷3");
                item.setIcon("普通格式刷");
                item.setSection("highlight_html");
                item.onClick(() => this.彩底格式刷(this.settings.bColor3,'荧光笔3'));
            });
            menu.addItem((item) => {
                item.setTitle("荧光笔4");
                //item.setIcon("格式刷4");
                item.setIcon("普通格式刷");
                item.setSection("highlight_html");
                item.onClick(() => this.彩底格式刷(this.settings.bColor4,'荧光笔4'));
            });
            menu.addItem((item) => {
                item.setTitle("荧光笔4");
                //item.setIcon("格式刷5");
                item.setIcon("普通格式刷");
                item.setSection("highlight_html");
                item.onClick(() => this.彩底格式刷(this.settings.bColor5,'荧光笔5'));
            });

            //如果Bt支持选项开启显示bt主题自带的语法效果
            if(this.settings.isBT)
            {
                menu.addItem((item) => {
                    item.setTitle("*==多彩高亮1==*");
                    item.setIcon("普通格式刷");
                    item.setSection("highlight");
                    item.onClick(() =>this.多彩高亮格式刷1());
                });
                menu.addItem((item) => {
                    item.setTitle("**==多彩高亮2==**");
                    item.setIcon("普通格式刷");
                    item.setSection("highlight");
                    item.onClick(() =>this.多彩高亮格式刷2());
                });
                menu.addItem((item) => {
                    item.setTitle("***==多彩高亮3==***");
                    item.setIcon("普通格式刷");
                    item.setSection("highlight");
                    item.onClick(() =>this.多彩高亮格式刷3());
                });
                menu.addItem((item) => {
                    item.setTitle("==~~涂黑~~==");
                    item.setIcon("普通格式刷");
                    item.setSection("highlight");
                    item.onClick(() =>this.涂黑格式刷());
                });
                menu.addItem((item) => {
                    item.setTitle("*==~~涂彩~~==*");
                    item.setIcon("普通格式刷");
                    item.setSection("highlight");
                    item.onClick(() =>this.涂彩格式刷());
                });
                menu.addItem((item) => {
                    item.setTitle("*~~挖空~~*");
                    item.setIcon("普通格式刷");
                    item.setSection("highlight");
                    item.onClick(() =>this.挖空格式刷());
                });
            }

            menu.showAtPosition({
                x: statusBarIconRect1.right + 5,
                y: statusBarRect1.top - 10,
            });
        });
    }


    onunload() {
        console.log('卸载插件');
    }

    async loadSettings() {
        console.log('加载了配置');
        console.log(this.app.customCss.themes);
        this.settings = Object.assign({}, DEFAULT_SETTINGS, await this.loadData());
     
    }
    async saveSettings() {
        await this.saveData(this.settings);
    }

    /** 以下为基础功能函数 */
    
    获取所选文本() {
        var cmEditor = this.获取编辑模式 ();
        if (!cmEditor) return;
        if (cmEditor.getSelection() == "") {
            return "";
        } else {
            return cmEditor.getSelection();
        }
    };
    
    获取笔记正文() {
        var cmEditor = this.获取编辑模式 ();
    	if (!cmEditor) return;
        //console.log("笔记正文\n"+cmEditor.getValue())
        return cmEditor.getValue();
    };

    替换所选文本(lines) {
        var cmEditor = this.获取编辑模式 ();
        if(cmEditor == null){
            return;
        }else{
            cmEditor.replaceSelection(lines);
        };
    };

    替换笔记正文(lines) {
        var cmEditor = this.获取编辑模式 ();
        if(cmEditor == null){
            return;
        }else{
            cmEditor.setValue(lines);
        };
     };

    获取编辑模式() {
        let view = this.app.workspace.getActiveViewOfType(obsidian.MarkdownView);
        if (!view){
            //return;
        };
        //let cmEditor = view.sourceMode.cmEditor;
        let cmEditor = view.editor;
        return cmEditor;
    };

    获取编辑器信息() {
        //初始信息获取，最基本函数
        编辑模式 = this.获取编辑模式 ();
        if(编辑模式 == null){return;};
        聚焦编辑 = 编辑模式.hasFocus();
        笔记全文 = 编辑模式.getDoc();   //此方法获取的笔记全文 是对象，不是文本
        //console.log("笔记全文\n"+笔记全文)
        笔记正文 = this.获取笔记正文(); //此处获取的是笔记内容的纯文本
        所选文本 = this.获取所选文本();
        当前光标 = 编辑模式.getCursor();
        当前行号 = 当前光标.line;
        当前行文本 = 编辑模式.getLine(当前行号);
        选至行首 = 编辑模式.getRange({line:当前行号,ch:0}, 当前光标);
        if(当前行文本!=""){
            选至行尾 = 编辑模式.getRange(当前光标,{line:当前行号,ch:当前行文本.length});
        }else{
            选至行尾 = 编辑模式.getRange(当前光标,{line:当前行号,ch:0});
        };        
        
        末行行号 = 编辑模式.lastLine();
        末行文本 = 编辑模式.getLine(末行行号);
        选至文首 = 编辑模式.getRange({line:0,ch:0},当前光标);
        if(末行文本!=""){
            选至文末 = 编辑模式.getRange(当前光标,{line:末行行号,ch:末行文本.length});
        }else{
            选至文末 = 编辑模式.getRange(当前光标,{line:末行行号,ch:0});
        };
    };

    /** 以下为自定义功能函数 */
    上次光标(){
        编辑模式.setCursor(历史光标);
    }
    
    光标跳转(方向) {
        this.获取编辑器信息 ();
        //var 选择字数=0;
        var 表达式;
        if(编辑模式 == null){return;};
        //new obsidian.Notice(所选文本+"\n"+当前行文本);
        if(所选文本 == ""){
            var 标题式1 = /^\s*# [^#]+$/;
            var 标题式2 = /^\s*## [^#]+$/;
            var 标题式3 = /^\s*### [^#]+$/;
            var 标题式4 = /^\s*#### [^#]+$/;
            var 标题式5 = /^\s*##### [^#]+$/;
            var 标题式6 = /^\s*###### [^#]+$/;
            var 列表式1 = /^(\- [^\[]|\d+\. ).*$/;
            var 列表式2 = /^(\s{4}|\t)(\- [^\[]|\d+\. ).*$/;
            var 列表式3 = /^(\s{8}|\t\t)(\- [^\[]|\d+\. ).*$/;
            var 列表式4 = /^(\s{12}|\t\t\t)(\- [^\[]|\d+\. ).*$/;
            var 待办式 = /^\s*\- \[[^\[\]]] .*$/;
            var 代码式 = /^```[^`]*$/;
            var 引用式 = /^\>.*$/;
            if(标题式1.test(当前行文本)){
                表达式 = 标题式1;
            }if(标题式2.test(当前行文本)){
                表达式 = 标题式2;
            }if(标题式3.test(当前行文本)){
                表达式 = 标题式3;
            }if(标题式4.test(当前行文本)){
                表达式 = 标题式4;
            }if(标题式5.test(当前行文本)){
                表达式 = 标题式5;
            }if(标题式6.test(当前行文本)){
                表达式 = 标题式6;
            }else if(待办式.test(当前行文本)){
                表达式 = 待办式;
            }else if(列表式1.test(当前行文本)){
                表达式 = 列表式1;
            }else if(列表式2.test(当前行文本)){
                表达式 = 列表式2;
            }else if(列表式3.test(当前行文本)){
                表达式 = 列表式3;
            }else if(列表式4.test(当前行文本)){
                表达式 = 列表式4;
            }else if(代码式.test(当前行文本)){
                表达式 = 代码式;
            }else if(引用式.test(当前行文本)){
                表达式 = 引用式;
            }else{
                return;
            }
            console.log('表达式 '+表达式);
            //逐行判断是否符合指定表达式
            for (var i=1;i<=末行行号;i++){
                var 新行号;
                if(方向=="下"){
                    新行号= 当前行号+i;
                }else if(方向=="上"){
                    新行号= 当前行号-i;
                };
                if(新行号<0 || 新行号>末行行号){
                    return;
                }
                var 临时行文本 = 编辑模式.getLine(新行号);
                if(表达式.test(临时行文本)){
                    编辑模式.setCursor({line:新行号,ch:临时行文本.length});
                    break
                };
            };
        }else{
            var 搜索范围="";
            var 加粗式 = /^\*\*[^\*]+\*\*$/;
            var 高亮式 = /^==[^=\n]+==$/;
            var 注释式 = /^%%[^%\n]*%%$/;
            var 删除式 = /^~~[^~]*~~$/;
            var 链接式 = /^\[\[[^\[\]]+\]\]$/;
            if(加粗式.test(所选文本)){
                表达式 = /\*\*[^\*]+\*\*/g;
            }else if(高亮式.test(所选文本)){
                表达式 = /==[^=]+==/g;
            }else if(注释式.test(所选文本)){
                表达式 = /%%[^%\n]*%%/g;
            }else if(删除式.test(所选文本)){
                表达式 = /~~[^~]*~~/g;
            }else if(链接式.test(所选文本)){
                表达式 = /\[\[[^\[\]]+\]\]/g;
            }else{
                表达式 = 所选文本;
            }
            console.log('表达式 '+表达式);
            选至文首 = 编辑模式.getRange({line:0,ch:0},当前光标);
            var 以前字数 = 选至文首.length;
            var 返回位置 = 0;
            var 搜索结果,起始位置,结束位置;
            if(方向=="下"){
                搜索范围 = 编辑模式.getRange(当前光标,{line:末行行号,ch:末行文本.length});
                返回位置 = 搜索范围.search(表达式);
                if(返回位置<0){
                    return
                };
                搜索结果 = 搜索范围.match(表达式)[0];
                //new obsidian.Notice(搜索结果.length);
                起始位置 = 以前字数+返回位置;
                结束位置 = 起始位置+搜索结果.length;
                编辑模式.setSelection({line:0,ch:起始位置}, {line:0,ch:结束位置});
            }else if(方向=="上"){
                搜索范围 = 编辑模式.getRange({line:0,ch:0},{line:当前光标.line,ch:当前光标.ch-所选文本.length});
                if(搜索范围.search(表达式)<0){
                    return
                };
                搜索结果 = 搜索范围.match(表达式).pop();
                返回位置 = 搜索范围.lastIndexOf(搜索结果);
                //new obsidian.Notice(搜索结果);
                结束位置 = 返回位置+搜索结果.length;
                编辑模式.setSelection({line:0,ch:返回位置}, {line:0,ch:结束位置});
            };
        }
    }

    async 开右窗口预览(){
        this.获取编辑器信息 ();
        await this.app.commands.executeCommandById('workspace:split-vertical');
        setTimeout("this.app.commands.executeCommandById('markdown:toggle-preview')", 200);
        setTimeout("this.app.commands.executeCommandById('editor:focus-left')", 200);
    }

    async 左窗向上滚动(){
        this.获取编辑器信息 ();
        //编辑模式.setLine(当前行号,"这是一行测试文本！使用setLine函数指定当前行内容");

        await this.app.commands.executeCommandById('editor:focus-left');
        let newWin = this.获取编辑模式 ();
        if(!newWin){
            await this.app.workspace.activeLeaf.view.editor.exec("goUp");
        }else{
            let yy = newWin.getScrollInfo().top;
            newWin.scrollTo(0,yy-this.settings.maxScroll);
        }
        

        await this.app.commands.executeCommandById('editor:focus-right');
        let oldpos = {line:当前光标.line,ch:当前光标.ch};
        await this.app.workspace.activeLeaf.view.editor.setSelection({line:0,ch:0},oldpos);
        await this.app.workspace.activeLeaf.view.editor.exec("goRight");
    }

    async 左窗向下滚动(){
        this.获取编辑器信息 ();

        await this.app.commands.executeCommandById('editor:focus-left');
        let newWin = this.获取编辑模式 ();
        if(!newWin){
            await this.app.workspace.activeLeaf.view.editor.exec("goDown");
        }else{
            let yy = newWin.getScrollInfo().top;
            newWin.scrollTo(0,yy+this.settings.maxScroll);
        }

        await this.app.commands.executeCommandById('editor:focus-right');
        let oldpos = {line:当前光标.line,ch:当前光标.ch};
        await this.app.workspace.activeLeaf.view.editor.setSelection({line:0,ch:0},oldpos);
        await this.app.workspace.activeLeaf.view.editor.exec("goRight");
    }

    关闭格式刷() {
        newNotice.hide();
        //关闭所有格式刷变量
        isText = false; //纯文本
        isBgC = false;  //多彩背景刷
        isCTxt = false; //多彩文字刷
        isGLS =false;   //==普通高亮==
		isGLS1 =false;   ///*==多彩高亮==*
		isGLS2 =false;   ///**==多彩高亮==**
		isGLS3 =false;   ///***==多彩高亮==***
        isCTS =false;   //**粗体**
        isXTS = false;  //*斜体*
        isSCS = false;  //~~删除线~~
        isXHS = false;
        isSB = false;
        isXB = false;
		
		isTHS = false; //涂黑
		isTCS = false; //涂彩
		isWKS = false; //挖空
        
        isbt1Txt = false;
        isbt2Txt = false;
    };

    游标上移() {
        this.获取编辑器信息 ();
        编辑模式.exec("goUp");
    };
    游标下移() {
        this.获取编辑器信息 ();
        编辑模式.exec("goDown");
    };
    游标左移() {
        this.获取编辑器信息 ();
        if(/(==|\*\*|~~|%%|\[\[|\]\])$/.test(选至行首)){
            编辑模式.exec("goLeft");
            编辑模式.exec("goLeft");
        }else{
            编辑模式.exec("goLeft");
        };
    };
    游标右移() {
        this.获取编辑器信息 ();
        if(/^(==|\*\*|~~|%%|\[\[|\]\])/.test(选至行尾)){
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else{
            编辑模式.exec("goRight");
        };
    };
    游标移至行首() {
        this.获取编辑器信息 ();
        编辑模式.setCursor({line:当前行号,ch:0});
    };
    游标移至行尾() {
        this.获取编辑器信息 ();
        编辑模式.setCursor({line:当前行号,ch:当前行文本.length});
    };
    游标移至文首() {
        this.获取编辑器信息 ();
        编辑模式.exec("goStart");
    };
    游标移至文末() {
        this.获取编辑器信息 ();
        编辑模式.exec("goEnd");
    };

    切换文件列表(_num) {
        this.获取编辑器信息 ();
        当前文件 = this.app.workspace.getActiveFile();
        当前文件路径 = 当前文件.path;
        var 父级文件夹 = 当前文件路径.replace(/[^\\\/]+$/,"");
        
        var 同级文件列表=[];
        this.app.vault.getMarkdownFiles().map((file) => {
            if(file.path==父级文件夹+file.basename+".md"){
                同级文件列表.push(file);
            }
        });
        同级文件列表 = 同级文件列表.sort(function (str1, str2) {
            return str1.path.localeCompare(str2.path, 'zh');
            });
        //new obsidian.Notice(同级文件列表.join("\n"));
        var thisID = 同级文件列表.indexOf(当前文件)+_num;
        if(thisID>同级文件列表.length-1){
            thisID=0;
        }else if(thisID<0){
            thisID=同级文件列表.length-1;
        }
        var xinFile = 同级文件列表[thisID];
        //new obsidian.Notice(父级文件夹+" "+thisID+" "+xinFile);
        this.app.workspace.activeLeaf.openFile(xinFile);
    };

    转换标签() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            return;
        };

        let tagReg = /(^|\s*)#([^#\s\/]+)(\s|[^\/]|\r*\n|$)/g;
	  	if (tagReg.test(所选文本)) {
            所选文本 = 所选文本.replace(tagReg, "$2");
		}else{
			所选文本 = " #" + 所选文本 + " ";
		}
        this.替换所选文本 (所选文本);        
    };

    标签双链互转() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            return;
        };

        let tagReg = /(^|\s*)#([^#\s\/]+)(\s|[^\/]|\r*\n|$)/g;
        let linkReg = /\[\[([^\[\]]+)\]\]/g;
	  	if (tagReg.test(所选文本)) {
            所选文本 = 所选文本.replace(tagReg, "[[$2]]");
		}else if(linkReg.test(所选文本)){
			所选文本 = 所选文本.replace(linkReg, " #$1 ");
		}
        this.替换所选文本 (所选文本);  
    };

    切换模式() {
        const noticeDuration = 10000;
        const isMarkdownView = this.app.workspace.getActiveViewOfType(obsidian.MarkdownView);
        if (!isMarkdownView) {
          new import_obsidian.Notice("没有常规的笔记或没有打开的文件。");
          return;
        }
        const activePane = this.app.workspace.activeLeaf;
        const currentView = activePane.getViewState();
        let currentMode;
        if (currentView.state.mode === "preview")
          currentMode = "preview";
        if (currentView.state.mode === "source" && currentView.state.source)
          currentMode = "source";
        if (currentView.state.mode === "source" && !currentView.state.source)
          currentMode = "live";
        const newMode = currentView;
        switch (currentMode) {
          case "preview":
            newMode.state.mode = "source";
            newMode.state.source = true;
            new obsidian.Notice("Now: Source Mode", noticeDuration);
            break;
          case "source":
            newMode.state.mode = "source";
            newMode.state.source = false;
            new obsidian.Notice("Now: Live Preview", noticeDuration);
            break;
          case "live":
            newMode.state.mode = "preview";
            new obsidian.Notice("Now: Reading Mode", noticeDuration);
            break;
        }
        activePane.setViewState(newMode);
      }

    转换内部链接() {
        let 提示语="";
        let 旧文本="";
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            let lreg = /\[\[([^\[\]]*)$/;
            let rreg = /^([^\[\]]*)\]\]/;
            if(lreg.test(选至行首) && rreg.test(选至行尾)){
                //当前光标前后有[[]]链接符号
                let 前文本 = 选至行首.match(lreg)[0];
                let 后文本 = 选至行尾.match(rreg)[0];
                编辑模式.setSelection({line:当前光标.line,ch:Number(当前光标.ch-前文本.length)}, {line:当前光标.line,ch:Number(当前光标.ch+后文本.length)});
                所选文本 = 编辑模式.getRange({line:当前光标.line,ch:Number(当前光标.ch-前文本.length)}, {line:当前光标.line,ch:Number(当前光标.ch+后文本.length)});
            }else{
                this.替换所选文本 ("[[");
                return;
            };
        };
        旧文本=所选文本;

        var link = /[\"\|\[\]\?\\\*\<\>\/:]/g;	//是否包含[]()及标点符号
        var link1 = /^([^\[\]]*)!\[+([^\[\]]*)$|^([^\[\]]*)\[+([^\[\]]*)$|^([^\[\]]*)\]+([^\[\]]*)$|^([^\[\]]*)\[([^\[\]]*)\]([^\[\]]*)$/g;	//是否只包含一侧的[[或]]
  		var link2 = /^[^\[\]]*(\[\[[^\[\]]*\]\][^\[\]]*)+$/g;	//是否包含N组成对的内链语法
  		var link4 = /([^\[\]\(\)\r\n]*)(\n*)(http.*)/mg;	//是否包含 说明文本&网址
	  	var link5 = /!?\[([^\[\]\r\n]*)(\n*)\]\((http[^\(\)]*)\)/mg;	//是否包含 [说明文本](网址)
  		var link8 = /([、\n])/g;
	  	if (link.test(所选文本)) {
	  		if (link1.test(所选文本)){
                new obsidian.Notice("划选内容不符合内链语法格式！");
	  			return;
	  		}else if (link2.test(所选文本)){
                提示语="内容包含内链语法格式，已经去除[[]]！";
	  			所选文本 = 所选文本.replace(/(\[\[(.*\|)*)/g,"");
                所选文本 = 所选文本.replace(/\]\]/g,"");
	  		}else if(link5.test(所选文本)){
	  			所选文本 = 所选文本.replace(link5,"$1$3");
                提示语="内容包含有[]()链接语法，已经去除符号！";
	  		}else if(link4.test(所选文本)){
	  			所选文本 = 所选文本.replace(link4,"[$1]($3)");
	  			所选文本 = 所选文本.replace("[\r\n]","");
                提示语="内容包含有说明文本和网址，已经转换！";
	  		}else{
                new obsidian.Notice("文件名不能包含下列字符:\*\"\\\/\<\>\:\|\?");
                return;
            }
		}else{
            提示语="内容未包含内链语法格式，需要转换";
			if (link8.test(所选文本)){
				所选文本 = 所选文本.replace(link8, "]]$1[[");
			}
			所选文本 = "[[" + 所选文本 + "]]";
		}
        console.log("您划选了 "+旧文本+"\n"+提示语);
        this.替换所选文本 (所选文本);        
    };

    转换潜在链接() {
        this.获取编辑器信息 ();
        编辑模式.exec("goStart");   //先移动光标，防止误选择
        
        let fileNameAry = [];
        if(this.settings.linkWords == ""){
            let 当前文件名 = this.app.workspace.getActiveFile().basename + ".md";
            for (const file of this.app.vault.getMarkdownFiles()) {
                if(file.name != 当前文件名){
                    fileNameAry.push(file.basename);
                    console.log(file.name)
                }
            };
            fileNameAry.sort().reverse();
        }else{
            fileNameAry = this.settings.linkWords.split("\n")
        }

        //将[[已有链接]]、[]()语法、#标签 或YAML内的字符追加⚘符号，防止被转换
        笔记正文 = 笔记正文.replace(/(?<=\[\[[^\[\]]+)([^\[\]])(?=[^\[\]]+\]\])/g,"$1⚘");
        笔记正文 = 笔记正文.replace(/(?<=\[[^\[\]]+)([^\[\]])(?=[^\[\]]+\]\(])/g,"$1⚘");
        笔记正文 = 笔记正文.replace(/(?<=#[^# ]*)([^#⚘ ])(?=([^# ]|$))/g,"$1⚘")
        笔记正文 = 笔记正文.replace(/(?<=[a-z0-9]+:[^\r\n]+)([^\r\n])(?=[^\r\n])/g,"$1⚘")
        
        let 转换链接内容 = [];
        let 累计 = 0;

        for (const oldStr of fileNameAry){
            if(oldStr.length<2){ continue } //不处理单字名称
            let xinStr = oldStr.replace(/([^⚘])/g,"$1⚘");  //⚘
            //将当前笔记中的潜在链接转为内部链接，如果有重复内容，只转换第一次出现的文本
            let 笔记新文 = 笔记正文.replace(oldStr,"[["+xinStr+"]]");
            //如果有重复内容，转换所有出现的文本
            //let 笔记新文 = 笔记正文.replace(new RegExp(oldStr,'g'),"[["+xinStr+"]]");
            
            if(笔记新文 != 笔记正文){
                笔记正文 = 笔记新文;
                累计 ++;
                转换链接内容.push(oldStr);
            }
        };

        if (累计 < 1) {
            new obsidian.Notice("📣 未发现潜在链接！")
            return;
        }else{
            笔记正文 = 笔记正文.replace(/⚘/g,"");
            this.替换笔记正文(笔记正文);
            new obsidian.Notice("📣 已转换"+累计+"个潜在链接：\n"+转换链接内容.join("\n")+"\n\n📣 按下 Ctrl+Z 撤消操作", 10000);
        }
    };

    转换同义链接() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            this.替换所选文本 ("[[");
            return;
        }
        var lNum = 所选文本.length +3
        var link = /[\"\|\[\]\?\\\*\<\>\/:\n]/g;	//是否包含[]()及标点符号
  	  	if (link.test(所选文本)) {
            return;
		}else{
			所选文本 = "[[|" + 所选文本 + "]]";
		}
        this.替换所选文本 (所选文本);
        
        var i=0;
        while (i<lNum){
            编辑模式.exec("goLeft");
            i++;
        }
    };

    粗体格式刷() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isCTS){
                isCTS = false;
                this.关闭格式刷();
                new obsidian.Notice("已关闭粗体格式刷！");
            }else{
                this.关闭格式刷();
                isCTS = true;
                newNotice = new obsidian.Notice("**粗体格式刷** 已打开！\n首尾不要选中标点。",0);
            };
        };
    };

    转换粗体() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isGLS){
                isGLS = false;
                newNotice.hide();
                new obsidian.Notice("已关闭粗体格式刷！");
            }
            笔记全文.replaceRange("****", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else{
            var link = /.*(\<b\>|\*\*)([^\*]*)(\<\/b\>|\*\*).*/g;	//是否包含加粗符号
            var link1 = /^[^\*](\<\/?b\>|\*\*)[^\*]*$/;	//是否只包含一侧的**
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现==符号");
            }else if (link.test(所选文本)){
                所选文本 = 所选文本.replace(/(\<\/?b\>|\*\*)/g,"");    //new obsidian.Notice("成对出现**符号");
                this.替换所选文本 (所选文本);
            }else{
                if(/^\<.*\>$/.test(所选文本) || /[,\.?!，。、？！]$/.test(所选文本)){
                    所选文本 = 所选文本.replace(/^/,"<b>");
                    所选文本 = 所选文本.replace(/$/,"</b>");
                }else{
                    所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1**$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1**$2");
                    所选文本 = 所选文本.replace(/^\*\*\*\*$/mg,"");
                }
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
    };

    高亮格式刷() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isGLS){
                this.关闭格式刷();
                new obsidian.Notice("已关闭高亮格式刷！");
            }else{
                this.关闭格式刷();
                isGLS = true;
                newNotice = new obsidian.Notice("==高亮格式刷== 已打开！",0);
            }
        }
    }
	多彩高亮格式刷1() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isGLS1){
                this.关闭格式刷();
                new obsidian.Notice("已关闭多彩高亮刷1！");
            }else{
                this.关闭格式刷();
                isGLS1 = true;
                newNotice = new obsidian.Notice("*==多彩高亮刷1==*  已打开！",0);
            }
        }
    }
	多彩高亮格式刷2() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isGLS2){
                this.关闭格式刷();
                new obsidian.Notice("已关闭彩高亮刷2！");
            }else{
                this.关闭格式刷();
                isGLS2 = true;
                newNotice = new obsidian.Notice("**==多彩高亮刷2==** 已打开！",0);
            }
        }
    }	
	多彩高亮格式刷3() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isGLS3){
                this.关闭格式刷();
                new obsidian.Notice("已关闭多彩高亮刷3！");
            }else{
                this.关闭格式刷();
                isGLS3 = true;
                newNotice = new obsidian.Notice("***==多彩高亮刷3==*** 已打开！",0);
            }
        }
    }
	
	涂黑格式刷() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isTHS){
                this.关闭格式刷();
                new obsidian.Notice("已关闭涂黑格式刷！");
            }else{
                this.关闭格式刷();
                isTHS = true;
                newNotice = new obsidian.Notice("==~~涂黑~~==已打开！",0);
            }
        }
    }
	涂彩格式刷() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isTCS){
                this.关闭格式刷();
                new obsidian.Notice("已关闭涂彩格式刷！");
            }else{
                this.关闭格式刷();
                isTCS = true;
                newNotice = new obsidian.Notice("*==~~涂彩~~==* 已打开！",0);
            }
        }
    }	
	挖空格式刷() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isWKS){
                this.关闭格式刷();
                new obsidian.Notice("已关闭挖空格式刷！");
            }else{
                this.关闭格式刷();
                isWKS = true;
                newNotice = new obsidian.Notice("*~~挖空~~* 已打开！",0);
            }
        }
    }
	
	
    转换高亮() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isGLS){
                isGLS = false;
                newNotice.hide();
                new obsidian.Notice("已关闭高亮格式刷！");
            }
            笔记全文.replaceRange("====", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else{
            var link = /==[^=]*==/;	//是否包含高亮符号
            var link1 = /^[^=]*==[^=]*$/;	//是否只包含一侧的==
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现==符号");
            }else if (link.test(所选文本)){
                所选文本 = 所选文本.replace(/==/g,"");    //new obsidian.Notice("成对出现==符号");
                this.替换所选文本 (所选文本);
            }else{
                所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1==$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1==$2");
                所选文本 = 所选文本.replace(/^====$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
    };
	
    转换高亮1() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isGLS1){
                isGLS1 = false;
                newNotice.hide();
                new obsidian.Notice("已关闭高亮格式刷！");
            }
            笔记全文.replaceRange("*====*", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
			编辑模式.exec("goRight");
        }else{
            var link = /\*==[^=]*==\*/;	//是否包含高亮符号
            var link1 = /^[^=]*==[^=]*$/;	//是否只包含一侧的==
			var isbank =/[ ]$/; //末尾是否有空格
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现*==符号");
            }else if (link.test(所选文本)){
				if (isbank.test(所选文本))
				所选文本 = 所选文本.replace(/\*==|==\*/g,"");
				else
				所选文本 = 所选文本.replace(/\ *==|\*==|==\* |==\*/g,""); //new obsidian.Notice("出现*== 或者==*符号");
                this.替换所选文本 (所选文本);
            }else{
				if (isbank.test(所选文本))
				所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1*==$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1==*$2");
				else
                所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1 *==$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1==* $2");
				
                所选文本 = 所选文本.replace(/^\*====\*$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
    };
	
    转换高亮2() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
             if(isGLS2){
                isGLS2 = false;
                newNotice.hide();
                new obsidian.Notice("已关闭高亮格式刷！");
            }
            笔记全文.replaceRange("**====**", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
			编辑模式.exec("goRight");
			编辑模式.exec("goRight");
        }else{
            var link = /\*\*==[^=]*==\*\*/;	//是否包含高亮符号
            var link1 = /^[^=]*==[^=]*$/;	//是否只包含一侧的==
			var isbank =/[ ]$/; //末尾是否有空格
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现**==符号");
            }else if (link.test(所选文本)){
				if (isbank.test(所选文本))
				所选文本 = 所选文本.replace(/\*\*==|==\*\*/g,"");
				else
				所选文本 = 所选文本.replace(/ \*\*==|\*\*==|==\*\* |==\*\*/g,""); //new obsidian.Notice("出现*== 或者==*符号");
                this.替换所选文本 (所选文本);
            }else{
				if (isbank.test(所选文本))
				所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1**==$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1==**$2");
				else
                所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1 **==$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1==** $2");
				
                所选文本 = 所选文本.replace(/^\*\*====\*\*$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
    };
		
    转换高亮3() {
        this.获取编辑器信息 ();
         if(所选文本 == ""){
             if(isGLS2){
                isGLS2 = false;
                newNotice.hide();
                new obsidian.Notice("已关闭高亮格式刷！");
            }
            笔记全文.replaceRange("***====***", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
			编辑模式.exec("goRight");
			编辑模式.exec("goRight");
			编辑模式.exec("goRight");
        }else{
            var link = /\*\*==[^=]*==\*\*/;	//是否包含高亮符号
            var link1 = /^[^=]*==[^=]*$/;	//是否只包含一侧的==
			var isbank =/[ ]$/; //末尾是否有空格
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现**==符号");
            }else if (link.test(所选文本)){
				if (isbank.test(所选文本))
				所选文本 = 所选文本.replace(/\*\*\*==|==\*\*\*/g,"");
				else
				所选文本 = 所选文本.replace(/ \*\*\*==|\*\*\*==|==\*\*\* |==\*\*\*/g,""); //new obsidian.Notice("出现*== 或者==*符号");
                this.替换所选文本 (所选文本);
            }else{
				if (isbank.test(所选文本))
				所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1***==$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1==***$2");
				else
                所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1 ***==$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1==*** $2");
				
                所选文本 = 所选文本.replace(/^\*\*\*====\*\*\*$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
    };
	
	 转换涂黑() {
        this.获取编辑器信息 ();
         if(所选文本 == ""){
             if(isTHS){
                isTHS = false;
                newNotice.hide();
                new obsidian.Notice("已关闭涂黑格式刷！");
            }
            笔记全文.replaceRange("==~~~~==", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
			编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else{
            var link = /==~~[^~=]*~~==/;	//是否包含高亮符号
            var link1 = /^[^=]*==[^=]*$/;	//是否只包含一侧的==
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现**==符号");
            }else if (link.test(所选文本)){
				所选文本 = 所选文本.replace(/==~~|==~~ |~~==/g,""); //new obsidian.Notice("出现*== 或者==*符号");
                this.替换所选文本 (所选文本);
            }else{
			
                所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1==~~$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1~~==$2");		
                所选文本 = 所选文本.replace(/^==~~~~==$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
    };
	 转换涂彩() {
        this.获取编辑器信息 ();
         if(所选文本 == ""){
             if(isTCS){
                isTCS = false;
                newNotice.hide();
                new obsidian.Notice("已关闭涂彩格式刷！");
            }
            笔记全文.replaceRange(" *==~~~~==* ", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
			编辑模式.exec("goRight");
            编辑模式.exec("goRight");
			编辑模式.exec("goRight");
			编辑模式.exec("goRight");
        }else{
            var link = /\*==~~[^=]*~~==\*/;	//是否包含高亮符号
            var link1 = /^[^=]*==[^=]*$/;	//是否只包含一侧的==
			var isbank =/[ ]$/; //末尾是否有空格
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现**==符号");
            }else if (link.test(所选文本)){
				if (isbank.test(所选文本))
				所选文本 = 所选文本.replace(/\*==~~|~~==\*/g,"");
				else
				所选文本 = 所选文本.replace(/\*==~~| \*==~~|~~==\* |~~==\*/g,""); //new obsidian.Notice("出现*== 或者==*符号");
                this.替换所选文本 (所选文本);
            }else{
				if (isbank.test(所选文本))
				所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1*==~~$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1~~==*$2");
				else
                所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1 *==~~$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1~~==* $2");
				
                所选文本 = 所选文本.replace(/^\*==~~~~==\*$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
    };
	 转换挖空() {
        this.获取编辑器信息 ();
         if(所选文本 == ""){
             if(isWKS){
                isWKS = false;
                newNotice.hide();
                new obsidian.Notice("已关闭挖空格式刷！");
            }
            笔记全文.replaceRange(" *~~~~* ", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
			编辑模式.exec("goRight");
        }else{
            var link = /\*~~[^~]*~~\*/;	//是否包含高亮符号
            var link1 = /^[^~]*~~[^~]*$/;	//是否只包含一侧的==
			var isbank =/[ ]$/; //末尾是否有空格
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现**==符号");
            }else if (link.test(所选文本)){
				if (isbank.test(所选文本))
				所选文本 = 所选文本.replace(/\*~~|~~\*/g,"");
				else
				所选文本 = 所选文本.replace(/ \*~~|\*~~|~~\* |~~\*/g,""); //new obsidian.Notice("出现*== 或者==*符号");
                this.替换所选文本 (所选文本);
            }else{
				if (isbank.test(所选文本))
				所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1*~~$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1~~*$2");
				else
                所选文本 = 所选文本.replace(/^([\t\s]*)([^\t\s])/mg,"$1 *~~$2").replace(/([^\t\s])([\t\s]*)$/mg,"$1~~* $2");
				
                所选文本 = 所选文本.replace(/^\*~~~~\*$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
    };
    
    斜体格式刷(){
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isXTS){
                this.关闭格式刷();
                new obsidian.Notice("已关闭格式刷！");
            }else{
                this.关闭格式刷();
                isXTS = true;
                newNotice = new obsidian.Notice("*斜体格式刷* 已打开！\n首尾不要选中标点。",0);
            }
        };
    };
    转换斜体() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isXTS){
                isXTS = false;
                newNotice.hide();
                new obsidian.Notice("已关闭斜体格式刷！");
            }
            笔记全文.replaceRange("**", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else{
            var link = /\*[^\*]*\*/;	//是否包含高亮符号
            var link1 = /^[^\*]*\*[^\*]*$/;	//是否只包含一侧的\*
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现\*符号");
            }else if (link.test(所选文本)){
                所选文本 = 所选文本.replace(/\*/g,"");    //new obsidian.Notice("成对出现\*符号");
                this.替换所选文本 (所选文本);
            }else{
                所选文本 = 所选文本.replace(/^(.*)$/mg,"\*$1\*");
                所选文本 = 所选文本.replace(/^\*\*$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
    };

    删除线格式刷(){
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isSCS){
                this.关闭格式刷();
                new obsidian.Notice("已关闭删除线格式刷！");
            }else{
                this.关闭格式刷();
                isSCS = true;
                newNotice = new obsidian.Notice("~~删除线格式刷~~ 已打开！",0);
            }
        }
    };
    转换删除线() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isSCS){
                isSCS = false;
                newNotice.hide();
                new obsidian.Notice("已关闭删除线格式刷！");
            }
            笔记全文.replaceRange("~~~~", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else{
            var link = /~~[^~]*~~/;	//是否包含删除线符号
            var link1 = /^[^~]*~~[^~]*$/;	//是否只包含一侧的~~
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现~~符号");
            }else if (link.test(所选文本)){
                所选文本 = 所选文本.replace(/~~/g,"");    //new obsidian.Notice("成对出现~~符号");
                this.替换所选文本 (所选文本);
            }else{
                所选文本 = 所选文本.replace(/^(.*)$/mg,"~~$1~~");
                所选文本 = 所选文本.replace(/^~~~~$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
        
    };

    下划线格式刷(){
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isXHS){
                this.关闭格式刷();
                new obsidian.Notice("已关闭下划线格式刷！");
            }else{
                this.关闭格式刷();
                isXHS = true;
                newNotice = new obsidian.Notice("<u>下划线格式刷</u> 已打开！",0);
            }
        }
    };

    转换下划线() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isXHS){
                isXHS = false;
                newNotice.hide();
                new obsidian.Notice("已关闭下划线格式刷！");
            }
            笔记全文.replaceRange("<u></u>", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else{
            var link = /\<u\>([^\<\>]*)\<\/u\>/mg;	//是否包含下划符号
            var link1 = /^[^\<\>]*\<\/?u\>[^\<\>]*$/;	//是否只包含一侧的<>
            if (link1.test(所选文本)){
                return; //new obsidian.Notice("只有一侧出现<>符号");
            }else if (link.test(所选文本)){
                所选文本 = 所选文本.replace(link,"$1");
                this.替换所选文本 (所选文本);
            }else{
                所选文本 = 所选文本.replace(/^(.*)$/mg,"<u>$1</u>");
                所选文本 = 所选文本.replace(/^\<u\>\<\/u\>$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        }
    };


    转换行内代码() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            笔记全文.replaceRange("``", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else{
            var link = /`[^`]*`/;	//是否包含代码行符号
            var link1 = /^[^`]*`[^`]*$/;	//是否只包含一侧的`
    
            if (link1.test(所选文本)){
                //new obsidian.Notice("只有一侧出现`符号");
                return;
            }else if (link.test(所选文本)){
                //new obsidian.Notice("成对出现`符号");
                所选文本 = 所选文本.replace(/`/g,"");
                this.替换所选文本 (所选文本);
            }else{
                //new obsidian.Notice("需要补充`符号");
                所选文本 = 所选文本.replace(/^(.*)$/mg,"`$1`");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        };
    };

    转换代码块() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            笔记全文.replaceRange("```\n\n```", 当前光标, 当前光标);
            编辑模式.exec("goDown");
        }else{
            var link = /```[^`]+```/;	//是否包含代码行符号
            var link1 = /^[^`]*```[^`]*$/m;	//是否只包含一侧的`
            所选文本 = 所选文本.replace(/\n/g,"↫");
            if (link1.test(所选文本)){
                //new obsidian.Notice("只有一侧出现```符号");
                return;
            }else if (link.test(所选文本)){
                //new obsidian.Notice("成对出现```符号");
                所选文本 = 所选文本.replace(/↫*```↫?|↫?```↫*/g,"");
                所选文本 = 所选文本.replace(/↫/g,"\n");
                this.替换所选文本 (所选文本);
            }else{
                //new obsidian.Notice("需要补充```符号");
                所选文本 = 所选文本.replace(/^(.*)$/m,"↫```↫$1↫```↫");
                所选文本 = 所选文本.replace(/↫/g,"\n");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goLeft");
                编辑模式.exec("goUp");
            }
        };
    };

    转换三浪线() {
        this.获取编辑器信息 ();
        var link = /~~~[^~]+~~~/;	//是否包含代码行符号
        var link1 = /^[^~]*~~~[^~]*$/m;	//是否只包含一侧的~
        if(所选文本 == ""){return};
        所选文本 = 所选文本.replace(/\n/g,"↫");
        if (link1.test(所选文本)){
            //new obsidian.Notice("只有一侧出现~~~符号");
            return;
        }else if (link.test(所选文本)){
            //new obsidian.Notice("成对出现~~~符号");
            所选文本 = 所选文本.replace(/~~~↫?|↫?~~~/g,"");
            所选文本 = 所选文本.replace(/↫/g,"\n");
            this.替换所选文本 (所选文本);
        }else{
            //new obsidian.Notice("需要补充~~~符号");
            所选文本 = 所选文本.replace(/^(.*)$/m,"~~~↫$1↫~~~");
            所选文本 = 所选文本.replace(/↫/g,"\n");
            this.替换所选文本 (所选文本);
            编辑模式.exec("goRight");
        }
    };

    上标格式刷(){
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isSB){
                this.关闭格式刷();
                new obsidian.Notice("已关闭上标格式刷！");
            }else{
                this.关闭格式刷();
                isSB = true;
                newNotice = new obsidian.Notice("<sup>上标格式刷</sup> 已打开！",0);
            }
        }
    };
    转换上标() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isSB){
                isSB = false;
                newNotice.hide();
                new obsidian.Notice("已关闭上标格式刷！");
            }
            笔记全文.replaceRange("<sup></sup>", 当前光标, 当前光标);
            编辑模式.setCursor({line:当前行号,ch:Number(当前光标.ch+5)});
        }else{
            var link = /\<sup\>[^\<\>]*\<\/sup\>/g;	//是否包含<sup>下标</sup>
            var link1 = /\<sup\>[^\<\>\/]*$|^[^\<\>]*\<\/sup\>/g;	//是否只包含一侧的<sup>下标</sup>
            if (link1.test(所选文本)){
                //new obsidian.Notice("只有一侧出现<sup>下标</sup>符号");
                return;
            }else if (link.test(所选文本)){
                所选文本 = 所选文本.replace(/(\<sup\>|\<\/sup\>)/g,"");
                this.替换所选文本 (所选文本);
            }else{
                所选文本 = 所选文本.replace(/^(.+)$/mg,"\<sup\>$1\<\/sup\>");
                所选文本 = 所选文本.replace(/^\<sup\>\s*\<\/sup\>$/mg,"");
                this.替换所选文本 (所选文本);
                编辑模式.exec("goRight");
            }
        };
    };

    下标格式刷(){
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isXB){
                this.关闭格式刷();
                new obsidian.Notice("已关闭下标格式刷！");
            }else{
                this.关闭格式刷();
                isXB = true;
                newNotice = new obsidian.Notice("<sub>下标格式刷</sub> 已打开！",0);
            }
        }
    };
    转换下标() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isXB){
                isXB = false;
                newNotice.hide();
                new obsidian.Notice("已关闭下标格式刷！");
            }
            笔记全文.replaceRange("<sub></sub>", 当前光标, 当前光标);
            编辑模式.setCursor({line:当前行号,ch:Number(当前光标.ch+5)});
        }else{
            var link = /\<sub\>[^\<\>]*\<\/sub\>/g;	//是否包含<sub>下标</sub>
            var link1 = /\<sub\>[^\<\>\/]*$|^[^\<\>]*\<\/sub\>/g;	//是否只包含一侧的<sub>下标</sub>
            if (link1.test(所选文本)){
                return;
            }else if (link.test(所选文本)){
                所选文本 = 所选文本.replace(/(\<sub\>|\<\/sub\>)/g,"");
            }else{
                所选文本 = 所选文本.replace(/^(.+)$/mg,"\<sub\>$1\<\/sub\>");
                所选文本 = 所选文本.replace(/^\<sub\>\s*\<\/sub\>$/mg,"");
            }
            this.替换所选文本 (所选文本);
            编辑模式.exec("goRight");
        };
    };

    转换填空() {
        var link = /\{\{c\d+::[^\{\}]+\}\}/ig;	//是否包含{{c*::}}
        var link1 = /\{\{c[^\{\}]*$|^[^\{\}]*\}\}/ig;	//是否只包含一侧的{{c*::}}
        var cs=[];
        var clinks="";
        var lastId=1;
        this.获取编辑器信息 ();
        
        if (link1.test(所选文本)){
            return;
        }else if (link.test(所选文本)){
            所选文本 = 所选文本.replace(/(\{\{c\d::|\}\})/g,"");
        }else{
            if(link.test(笔记正文)){
                cs = 笔记正文.match(link);
                clinks = cs.toString().replace(/\{\{c/ig,"").replace(/::[^\}]+\}\}/ig,"");
                new obsidian.Notice(clinks);
                //lastId = Number(clinks.split(",").sort().pop())+1;
                lastId = Number(clinks.split(",").sort((a, b) => Number(a) - Number(b)).pop()) + 1;
                new obsidian.Notice(lastId);
            }
            所选文本 = 所选文本.replace(/^(.+)$/m,"{{c"+lastId+"::$1}}");
        }
        this.替换所选文本 (所选文本);
    };

    选择当前整段 () {    
        this.获取编辑器信息 ();
        if(当前行文本!=""){
            编辑模式.setSelection({line:当前行号,ch:0}, {line:当前行号,ch:当前行文本.length});
        };
    };

    选择当前整句 () {
        this.获取编辑器信息 ();
        var 句前 = 选至行首.match(/(?<=(^|[。？！]))[^。？！]*$/);
        var 句后 = 选至行尾.match(/^[^。？！]*([。？！]|$)/);
        if(句前==null && 句后==null){
            编辑模式.setSelection({line:当前行号,ch:0}, {line:当前行号,ch:当前行文本.length});
        }else{
            var _length1 = 选至行首.length-句前[0].length;
            var _length2 = 选至行首.length+句后[0].length;
            //new obsidian.Notice(句前+"\n光标\n"+句后);
            编辑模式.setSelection({line:当前行号,ch:_length1}, {line:当前行号,ch:_length2});
        }
    };

    选择当前语法 () {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            var 句前 = 选至行首.match(/(^|\*\*|==|~~|%%|\[\[)[^\*=~%\[\]]*$/);
            var 句后 = 选至行尾.match(/^[^\*=~%\[\]]*(\*\*|==|~~|%%|\]\]|$)/);
            if(句前==null ||句后==null){
                return;
            }else{
                var _length1 = 选至行首.length-句前[0].length;
                var _length2 = 选至行首.length+句后[0].length;
                //new obsidian.Notice(句前+"\n光标\n"+句后);
                编辑模式.setSelection({line:当前行号,ch:_length1}, {line:当前行号,ch:_length2});
            };
        }else if(/^(\*\*|==|~~|%%|\[\[)[^\*=~%\[\]]*(\*\*|==|~~|%%|\]\])$/.test(所选文本)){
            this.选择当前整句 ();
        }
    };

    重复当前行 () {
        this.获取编辑器信息 ();
        var 新行文本 = "\n" + 当前行文本;
        笔记全文.replaceRange(新行文本, {line:当前行号,ch:当前行文本.length}, {line:当前行号,ch:当前行文本.length});
    };

    智能符号 () {
        this.获取编辑器信息 ();
        let 转换文本 = "";
        var 标前两字 = 编辑模式.getRange({line:当前行号,ch:选至行首.length-2}, 当前光标);
        var 标后两字 = 编辑模式.getRange(当前光标,{line:当前行号,ch:选至行首.length+2});
        //new obsidian.Notice("标前两字\n"+标前两字+"\n\n标后两字：\n"+标后两字);

        if(选至行尾.match(/^(\]\]|\=\=|\*\*|\~\~)/)){
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");   //如果下个字符是后括号，则跃过
        }else if(选至行尾.match(/^[$》〉］｝】〗〕』」）}\)]/)){
            编辑模式.exec("goRight");   //如果下个字符是后括号，则跃过
        }else if(标前两字.match(/^[【\[][（\(]$/)){
            笔记全文.replaceRange("〖", {line:当前行号,ch:选至行首.length-2}, 当前光标);
        }else if(标前两字.match(/^[（\(][《\<]$/)){
            笔记全文.replaceRange("〈", {line:当前行号,ch:选至行首.length-2}, 当前光标);
        }else if(标前两字.match(/^[\(（][【\[]$/)){
            笔记全文.replaceRange("〔", {line:当前行号,ch:选至行首.length-2}, 当前光标);
        }else if(标前两字.match(/^[“\"][【\[]$/)){
            笔记全文.replaceRange("『", {line:当前行号,ch:选至行首.length-2}, 当前光标);
        }else if(标前两字.match(/^[‘\'][【\[]$/)){
            笔记全文.replaceRange("「", {line:当前行号,ch:选至行首.length-2}, 当前光标);
        }else if(标前两字.match(/^……$/)){
            笔记全文.replaceRange("^", {line:当前行号,ch:选至行首.length-2}, 当前光标);
        }else if(标前两字.match(/^￥￥$/)){
            笔记全文.replaceRange("$$", {line:当前行号,ch:选至行首.length-2}, 当前光标);
            编辑模式.exec("goLeft");
        }else if(选至行首.match(/《[^《》〈〉｛｝【】〖〗〔〕（）『』「」]*$/)){
            笔记全文.replaceRange("》", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/〈[^《》〈〉［］｛｝【】〖〗〔〕（）『』「」]*$/)){
            笔记全文.replaceRange("〉", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/［[^《》〈〉［］｛｝【】〖〗〔〕（）『』「」]*$/)){
            笔记全文.replaceRange("］", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/｛[^《》〈〉［］｛｝【】〖〗〔〕（）『』「」]*$/)){
            笔记全文.replaceRange("｝", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/【[^《》〈〉［］｛｝【】〖〗〔〕（）『』「」]*$/)){
            笔记全文.replaceRange("】", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/〖[^《》〈〉［］｛｝【】〖〗〔〕（）『』「」]*$/)){
            笔记全文.replaceRange("〗", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/〔[^《》〈〉［］｛｝【】〖〗〔〕（）『』「」]*$/)){
            笔记全文.replaceRange("〕", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/『[^《》〈〉［］｛｝【】〖〗〔〕（）『』「」]*$/)){
            笔记全文.replaceRange("』", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/「[^《》〈〉［］｛｝【】〖〗〔〕（）『』「」]*$/)){
            笔记全文.replaceRange("」", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/（[^《》〈〉［］｛｝【】〖〗〔〕（）『』「」]*$/)){
            笔记全文.replaceRange("）", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/^[》、](.*)$/)){
            转换文本 = 选至行首.replace(/^》(.*)$/,">$1");
            转换文本 = 转换文本.replace(/^、(.*)$/,"/$1");
            笔记全文.replaceRange(转换文本, {line:当前行号,ch:0}, 当前光标);
        }else if(选至行首.match(/\[\[[^=\[\]\*~]*$/)){
            笔记全文.replaceRange("]]", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else if(选至行首.match(/\$\$[^\$]*$/)){
            笔记全文.replaceRange("$$", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else if(选至行首.match(/\$[^\$]*$/)){
            笔记全文.replaceRange("$", 当前光标, 当前光标);
            编辑模式.exec("goRight");
        }else if(选至行首.match(/==[^=\[\]\*~]*$/)){
            笔记全文.replaceRange("==", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else if(选至行首.match(/\*\*[^=\[\]\*~]*$/)){
            笔记全文.replaceRange("**", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else if(选至行首.match(/%%[^=\[\]\*~%]*$/)){
            笔记全文.replaceRange("%%", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else if(选至行首.match(/~~[^=\[\]\*~]*$/)){
            笔记全文.replaceRange("~~", 当前光标, 当前光标);
            编辑模式.exec("goRight");
            编辑模式.exec("goRight");
        }else{
            let coreAry = ["query|qy","mermaid|mm","dataview|dv","ABAP","apl","asciiarmor","ASN.1","asp","assembly","bash","basic","C","C#","cassandra","ceylon","clike","clojure","cmake","cobol","coffeescript","commonlisp","cpp","CQL","crystal","csharp","css","cypher","cython","D","dart","diff","django","dockerfile","ejs","elixir","elm","embeddedjs","erb","erlang","F#","flow","forth","fortran","fsharp","gas","gfm","gherkin","gist","go","groovy","handlebars","haskell","haxe","html","http","hxml","idl","ini","jade","java","Javascript|js","jinja2","json","jsp","jsx","julia","kotlin","latex","less","lisp","livescript","lua","makefile","mariadb","markdown|md","mathematica","matlab","mbox","mermaid","mssql","mysql","nginx","nim","nsis","objc","objective-c","ocaml","octave","oZ","pascal","perl","perl6","pgp","php","php+HTML","plsql","powershell","properties","protobuf","pseudocode","python|py","q","R","react","reStructuredText","rst","ruby","rust","SAS","scala","scheme","SCSS","sequence","sh","shell","smalltalk","solidity","SPARQL","spreadsheet","sql","sqlite","squirrel","stylus","swift","tcl","tex","tiddlywiki","tiki","wiki","toml","tsx","turtle","twig","typescript","V","vbscript","velocity","verilog","vhdl","vb|visual basic","vue","web-idl","xaml","xml","xml-dtd","xquery","yacas","yaml"];
            for(var _l=0; _l<coreAry.length; _l++){
                let _coreID = coreAry[_l];
                let qcStr = _coreID.replace(/\|.+$/,"");
                if(eval("/^"+_coreID+"$/i").test(选至行首)){
                    笔记全文.replaceRange("```"+qcStr+"\n\n```\n", {line:当前行号,ch:0}, 当前光标);
                    编辑模式.exec("goLeft");
                    编辑模式.exec("goUp");
                };
            };

            let infoAry = ["note|笔记|记录","abstract|摘要","summary|总结","tldr|概要","info|信息|资讯","todo|待办","tip|hint|提示","important|重要","success|成功","check|检查","done|完成","question|问题","help|帮助","faq|常见问题","warning|警告","caution|提醒","attention|注意","failure|失败","fail|丢失","missing|缺失","danger|危险","error|错误","bug|漏洞|缺陷","example|示例|例子","quote|cite|引用"];
            for(var i=0; i<infoAry.length; i++){
                let _strID = infoAry[i];
                let oneStr = _strID.replace(/\|.+$/,"");
                if(eval("/^("+_strID+")( ([左中右lcr]|left|center|right))*[\+\-]*$/i").test(选至行首)){
                    let calloutStr = ">[!"+oneStr+"■]▲ \n>";
                    new obsidian.Notice("符合表达式，可以转换！");
                    if(/\s([左l]|left)(?=[\+\-]|$)/i.test(选至行首)){
                        calloutStr = calloutStr.replace("■"," left");
                    }else if(/\s([中c]|center)(?=[\+\-]|$)/i.test(选至行首)){
                        calloutStr = calloutStr.replace("■"," center");
                    }else if(/\s([右r]|right)(?=[\+\-]|$)/i.test(选至行首)){
                        calloutStr = calloutStr.replace("■"," right");
                    }else{
                        calloutStr = calloutStr.replace("■","");
                    }
                    if(/\+$/.test(选至行首)){
                        calloutStr = calloutStr.replace("▲","+");
                    }else if(/\-$/.test(选至行首)){
                        calloutStr = calloutStr.replace("▲","-");
                    }else{
                        calloutStr = calloutStr.replace("▲","");
                    }

                    笔记全文.replaceRange(calloutStr, {line:当前行号,ch:0}, 当前光标);
                    编辑模式.exec("goLeft");
                    编辑模式.exec("goLeft");
                }
            };
        };
    };

    标题语法(_str) {
        let link = eval("/^"+_str+" ([^#]+)/");	//是否包含几个#符号
        this.获取编辑器信息 ();
        let 新文本 = "";
        
        if(_str==""){   //若为标题，转为普通文本
            新文本 = 当前行文本.replace(/^(\>*(\[[!\w]+\])?\s*)#+\s/,"$1");
        }else{  //列表、引用，先转为普通文本，再转为标题
            新文本 = 当前行文本.replace(/^\s*(#*|\>|\-|\d+\.)\s*/m,"");
            console.log(新文本);
            新文本 = _str+" "+新文本;
        }
        //笔记全文.replaceRange(新文本, {line:当前行号,ch:0}, {line:当前行号,ch:当前行文本.length});
        编辑模式.setLine(当前行号,新文本);
        编辑模式.setCursor({line:当前行号,ch:Number(新文本.length-选至行尾.length)});
    };

    调节标题级别(增加) {
        this.获取编辑器信息 ();
        let 新文本 = "";
        let 位置 = 选至行首.length;
        if(增加){
            if(/^##+\s/.test(当前行文本)){
                新文本 = 当前行文本.replace(/(^\s*)##/,"$1#");
                位置--
            }else{
                return;
            }
        }else{
            if(/^#{1,5}\s/.test(当前行文本)){
                新文本 = 当前行文本.replace(/(^[‌‌‌‌‌‌　]*)#/,"$1##");
                位置++
            }else{
                return;
            }
        }
        //笔记全文.replaceRange(新文本, {line:当前行号,ch:0}, {line:当前行号,ch:当前行文本.length});
        编辑模式.setLine(当前行号,新文本);
        编辑模式.setCursor({line:当前行号,ch:位置});
    };

    彩字格式刷(_color){
        this.关闭格式刷();
        isCTxt = true;
        this.settings.hColor = _color;
        newNotice = new obsidian.Notice("多彩文字格式刷 已打开！",0);
    }

    转换文字颜色() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){return};

        let _html0 = /\<font color=[0-9a-zA-Z#]+[^\<\>]*\>[^\<\>]+\<\/font\>/g;
        let _html1 = /^\<font color=[0-9a-zA-Z#]+[^\<\>]*\>([^\<\>]+)\<\/font\>$/;
        let _html2 = '\<font color='+this.settings.hColor+'\>$1\<\/font\>';
        let _html3 = /\<font color=[^\<]*$|^[^\>]*font\>/g;	//是否只包含一侧的<>

        if (_html3.test(所选文本)){
            return; //new obsidian.Notice("不能转换颜色！");
        }else if (_html0.test(所选文本)){
            if(_html1.test(所选文本)){
                //new obsidian.Notice("替换颜色！");
                所选文本 = 所选文本.replace(_html1,_html2);
            }else{
                所选文本 = 所选文本.replace(/\<font color=[0-9a-zA-Z#]+[^\<\>]*?\>|\<\/font\>/g,""); 
            }
        }else{
            所选文本 = 所选文本.replace(/^(.+)$/mg,_html2);  //new obsidian.Notice("可以转换颜色！");
        }
        this.替换所选文本 (所选文本);
        编辑模式.exec("goRight");
    };

    彩底格式刷(_color){
        this.关闭格式刷();
        isBgC = true;
        this.settings.bColor = _color;
        newNotice = new obsidian.Notice("多彩背景格式刷 已打开！",0);
    }
    转换背景颜色() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        let _html0 = /\<span style=[\"'][^\<\>]+:[0-9a-zA-Z#]+[\"'][^\<\>]*\>[^\<\>]+\<\/span\>/g;
        let _html1 = /^\<span style=[\"'][^\<\>]+:[0-9a-zA-Z#]+[\"'][^\<\>]*\>([^\<\>]+)\<\/span\>$/;
        let _html2 = '\<span style=\"background\:'+this.settings.bColor+'\"\>$1\<\/span\>';
        let _html3 = /\<span style=[^\<]*$|^[^\>]*span\>/g;	//是否只包含一侧的<>
        if (_html3.test(所选文本)){
            return; //new obsidian.Notice("不能转换颜色！");
        }else if (_html0.test(所选文本)){
            if(_html1.test(所选文本)){
                所选文本 = 所选文本.replace(_html1,_html2); 
            }else{
                所选文本 = 所选文本.replace(/\<span style=[\"'][^\<\>]+:[0-9a-zA-Z#]+[\"'][^\<\>]*\>|\<\/span\>/g,"");
                //new obsidian.Notice("需要去除颜色！");
            }
        }else{
            所选文本 = 所选文本.replace(/^(.+)$/mg,_html2);  //new obsidian.Notice("可以转换颜色！");
        }
        this.替换所选文本 (所选文本);
        编辑模式.exec("goRight");
    };
    
    转换无语法文本() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            if(isText){
                this.关闭格式刷();
                new obsidian.Notice("已关闭格式刷！");
            }else{
                this.关闭格式刷();
                isText = true;
                newNotice = new obsidian.Notice("纯文本格式刷 已打开！",0);
                //new obsidian.Notice("请先划选部分文本，再执行命令！");
                let reg1 = /(~~|%%|\*==|==|\*\*?|\<[^\<\>]*?\>|!?\[\[*|`|_|!?\[)([^!#=\[\]\<\>\`_\*~\(\)]*)$/;
                let reg2 = /^([^!=\[\]\<\>\`_\*~\(\)]*)(~~|%%|==\*|==|\*\*?|\<[^\<\>]*\>|\]\]|`|_|\]\([^\(\)\[\]]*\))/;
                if(选至行首.match(reg1)!=null && 选至行尾.match(reg2)!=null){
                    选至行首 = 选至行首.replace(reg1,"$2");
                    选至行尾 = 选至行尾.replace(reg2,"$1");
                    //笔记全文.replaceRange(选至行首+选至行尾, {line:当前行号,ch:0},{line:当前行号,ch:当前行文本.length});
                    编辑模式.setLine(当前行号,选至行首+选至行尾);
                    编辑模式.setCursor({line:当前行号,ch:Number(选至行首.length)});
                }
            }
        }else{
            let mdText = /(^#+\s|(?<=^|\s*)#|^\>|^\- \[( |x)\]|^\+ |\<[^\<\>]+?\>|^1\. |^\s*\- |^\-+$|^\*+$|==\*|\*==|==\*\*|\*\*==|==\*\*\*|\*\*\*==)/mg;
            所选文本 = 所选文本.replace(mdText,"");
            所选文本 = 所选文本.replace(/^[ ]+|[ ]+$/mg,"");
            所选文本 = 所选文本.replace(/\!?\[\[([^\[\]\|]*\|)*([^\(\)\[\]]+)\]\]/g,"$2");
            所选文本 = 所选文本.replace(/\!?\[+([^\[\]\(\)]+)\]+\(([^\(\)]+)\)/g,"$1");
            所选文本 = 所选文本.replace(/`([^`]+)`/g,"$1");
            所选文本 = 所选文本.replace(/_([^_]+)_/g,"$1");
            所选文本 = 所选文本.replace(/==([^=]+)==/g,"$1");
            所选文本 = 所选文本.replace(/\*\*?([^\*]+)\*\*?/g,"$1");
            所选文本 = 所选文本.replace(/~~([^~]+)~~/g,"$1");
            所选文本 = 所选文本.replace(/(\r*\n)+/mg,"\r\n");
       
			this.替换所选文本 (所选文本);
        }
    };

    内链转为超链接() {
		this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        所选文本 = 所选文本.replace(/\[\[([^\[\]]+)(\.\w{3,4})?\]\]/g,"[$1]($1$2)");
        所选文本 = 所选文本.replace(/(?<=\]\([^\)]*)\s(?=[^\(]*\))/g,"%20");
        this.替换所选文本 (所选文本);
    };

    超链接转为内链() {
		this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        所选文本 = 所选文本.replace(/\[([^\[\]]+)\]\([^\(\)]+\)/g,"[[$1]]");
        this.替换所选文本 (所选文本);
    };

    去除超链接语法() {
		this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        所选文本 = 所选文本.replace(/\[([^\[\]]+)\]\([^\(\)]+\)/g,"$1");
        this.替换所选文本 (所选文本);
    };

    转换引号() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        if(/[「『』」]/g.test(所选文本)){
            所选文本 = 所选文本.replace(/「/g,"“");
            所选文本 = 所选文本.replace(/」/g,"”");
            所选文本 = 所选文本.replace(/『/g,"‘");
            所选文本 = 所选文本.replace(/』/g,"’");
        }else{
            所选文本 = 所选文本.replace(/“/g,"「");
            所选文本 = 所选文本.replace(/”/g,"」");
            所选文本 = 所选文本.replace(/‘/g,"『");
            所选文本 = 所选文本.replace(/’/g,"』");
        }
        this.替换所选文本 (所选文本);
    };

    /*
    括选文本1() {
        let link = /(.*【[^【】]+】.*)/g;	//是否包含【】
        let link1 = /【[^【】]*$|^[^【】]*】/g;	//是否只包含一侧的【】
        this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        if (link1.test(所选文本)){
            return;
        }else if (link.test(所选文本)){
            所选文本 = 所选文本.replace(/[【】]/g,"");
        }else{
            所选文本 = 所选文本.replace(/^(.+)$/mg,"【$1】");
            所选文本 = 所选文本.replace(/^【\s*】$/mg,"");
        }
        this.替换所选文本 (所选文本);
    };

    括选文本2() {
        let link = /(.*（[^（）]*）.*)/g;	//是否包含【】
        let link1 = /（[^（）]*$|^[^（）]*）/g;	//是否只包含一侧的【】
        this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        if (link1.test(所选文本)){
            return;
        }else if (link.test(所选文本)){
            所选文本 = 所选文本.replace(/[（）]/g,"");
        }else{
            所选文本 = 所选文本.replace(/^(.+)$/mg,"($1)");
            所选文本 = 所选文本.replace(/^(\s*)$/mg,"");
        }
        this.替换所选文本 (所选文本);
    };

    括选文本3() {
        let link = /(.*「[^「」]*」.*)/g;	//是否包含「」
        let link1 = /「[^「」]*$|^[^「」]*」/g;	//是否只包含一侧的「
        this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        if (link1.test(所选文本)){
            return;
        }else if (link.test(所选文本)){
            所选文本 = 所选文本.replace(/[「」]/g,"");
        }else{
            所选文本 = 所选文本.replace(/^(.+)$/mg,"「$1」");
            所选文本 = 所选文本.replace(/^「\s*」$/mg,"");
        }
        this.替换所选文本 (所选文本);
    };

    括选文本4() {
        let link = /(.*《[^《》]*》.*)/;	//是否包含《》
        let link1 = /《[^《》]*$|^[^《》]*》/;	//是否只包含一侧的《
        this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        if (link1.test(所选文本)){
            return;
        }else if (link.test(所选文本)){
            所选文本 = 所选文本.replace(/[《》]/g,"");
        }else{
            所选文本 = 所选文本.replace(/^(.+)$/mg,"《$1》");
            所选文本 = 所选文本.replace(/^《\s*》$/mg,"");
        }
        this.替换所选文本 (所选文本);
    };
    */

    删除当前段落() {
        //优化Ob自带功能，支持删除光标所在链接文本，修正有序列表的序号
        this.获取编辑器信息 ();
        let reg = /^[\t\s]*\d+(?=\.\s[^\s])/mg;
        if(当前行文本.match(reg)==null){
            console.log("当前不是列表");
            let reg1 =/^(.*\[\[)[^\[]+$/;
            let reg2 = /^[^\]]+(\]\].*)$/;

            //当前光标在[[]]中间，只删除链接文字
            if(reg1.test(选至行首) && reg2.test(选至行尾)){
                new obsidian.Notice("优先删除内部链接，可以切换标题！");
                选至行首 = 选至行首.replace(reg1,"$1");
                选至行尾 = 选至行尾.replace(reg2,"$1");
                //笔记全文.replaceRange(选至行首+选至行尾, {line:当前行号,ch:0},{line:当前行号,ch:当前行文本.length});
                编辑模式.setLine(当前行号,选至行首+选至行尾);
                编辑模式.setCursor({line:当前行号,ch:选至行首.length});
            }else{
                //当前所在行为普通文本，直接删除
                笔记全文.replaceRange("", {line:当前行号,ch:0},{line:Number(当前行号+1),ch:0});
            }
        }else{
            //当前所在行为有序列表的一项，则调小后表部分的序号
            选至文末 = 选至文末.replace(/\n/g,"↫");
            let 后表部分 = 选至文末.match(/^([\t\s]*\d+\.\s[^↫]*↫)+/)[0].replace(/↫/g,"\n"); //替换为单行文本，再截取后表部分
            let 后表行数 = 后表部分.match(/\n/g).length;    //计算换行次数
            let 缩进字符 = 当前行文本.match(/^[\t\s]*(?=\d)/)[0];
            let reg3 = eval("/(?<=^"+缩进字符+")\\d+(?=\\.*\\s)/mg");    //按当前行的缩进格式进行查找替换
            后表部分 = 后表部分.replace(reg3, function(a){return a*1-1;});
            后表部分 = 后表部分.replace(/^[^\n]*\n/,"");
            //new obsidian.Notice("选至文末\n"+选至文末);
            笔记全文.replaceRange(后表部分, {line:当前行号,ch:0},{line:当前行号+后表行数,ch:编辑模式.getLine(当前行号+后表行数).length});
        };
    };

    有转无序列表() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        所选文本 = 所选文本.replace(/(?<=^\s*)[0-9]+\.\s/mg,"- ");
        this.替换所选文本 (所选文本);
    };

    无转有序列表() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        所选文本 = 所选文本.replace(/(?<=^\s*)[\-\+]\s/mg,"1. ");
        this.替换所选文本 (所选文本);
    };
    
    转换待办列表() {
        this.获取编辑器信息 ();
        let 当前新文本 = 当前行文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[) (?=\]\s[^\s])/mg,"x☀");
        当前新文本 = 当前新文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[)x(?=\]\s[^\s])/mg,"-☀");
        当前新文本 = 当前新文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[)\-(?=\]\s[^\s])/mg,"!☀");
        当前新文本 = 当前新文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[)\!(?=\]\s[^\s])/mg,"?☀");
        当前新文本 = 当前新文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[)\?(?=\]\s[^\s])/mg,">☀");
        当前新文本 = 当前新文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[)\>(?=\]\s[^\s])/mg,"<☀");
        当前新文本 = 当前新文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[)\<(?=\]\s[^\s])/mg,"+☀");
        当前新文本 = 当前新文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[)\+(?=\]\s[^\s])/mg," ☀");
        当前新文本 = 当前新文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[[\sx\-\+\?\!\<\>])☀(?=\]\s[^\s])/mg,"");
        //笔记全文.replaceRange(当前新文本, {line:当前行号,ch:0},{line:当前行号,ch:当前行文本.length});
        编辑模式.setLine(当前行号,当前新文本);
    };

    转换callout语法(){
        this.获取编辑器信息 ();
        if(所选文本 == ""){return};
        所选文本 = 所选文本.replace(/\n/g,"↫");
        if(所选文本.includes(">[!")){
            //new obsidian.Notice("需要去除>符号");
            所选文本 = 所选文本.replace(/↫?>\[![^↫]+(?=↫)/m,"");
            所选文本 = 所选文本.replace(/↫\>/g,"\n");
        }else{
            //new obsidian.Notice("需要补充>符号");
            所选文本 = 所选文本.replace(/^(.*)$/m,">[!note]↫$1");
            所选文本 = 所选文本.replace(/↫/g,"\n>");
        }        
        this.替换所选文本 (所选文本);
        编辑模式.exec("goRight");
    };

    合计任务用时() {
        this.获取编辑器信息 ();
        let 当前缩进 = "";
        let 父级缩进 = "";
        if(/^\s*\- [^\n]+$/.test(当前行文本)){
            当前缩进 = 当前行文本.replace(/\-.*$/m,"");
        }

        let timeReg = /.*(20\d\d\-\d\d\-\d\d \d\d:\d\d)\s*\-\s*(20\d\d\-\d\d\-\d\d \d\d:\d\d)\s*$/m;
        if(timeReg.test(当前行文本)){
            let startTime = 当前行文本.replace(timeReg,"$1");
            let stopTime = 当前行文本.replace(timeReg,"$2");
            let date1 = new Date(startTime.replace(/-/g, '/'));
            let date2 = new Date(stopTime.replace(/-/g, '/'));
        
            // 有三种方式获取，在后面会讲到三种方式的区别
            let time1 = date1.getTime();
            let time2 = date2.valueOf();

            new obsidian.Notice("当前任务用时 "+Number((time2-time1)/1000)+" 秒");
        }
        
    }

    标记完成及时间() {
        this.获取编辑器信息 ();
        let 当前新文本 = 当前行文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[) (?=\]\s[^\s])/mg,"x☀");
        当前新文本 = 当前新文本.replace(/(?<=^\s*([\-\+]|[0-9]+\.)\s\[[\sx\-\+\?\!\<\>])☀(?=\]\s[^\s])/mg,"");
        当前新文本 = 当前新文本+" "+this.生成时间戳();
        //笔记全文.replaceRange(当前新文本, {line:当前行号,ch:0},{line:当前行号,ch:当前行文本.length});
        编辑模式.setLine(当前行号,当前新文本);
    };

    自动设置标题() {    //修复 标题行首的空格或制表符影响正常格式 的问题。 20220807
		this.获取编辑器信息 ();
        编辑模式.exec("goStart");

        if (!笔记正文) return;
        笔记正文 = 笔记正文.replace(/\r?\n/g,"↫");
        笔记正文 = 笔记正文.replace(/↫\s*↫/g,"↫↫");
        笔记正文 = 笔记正文.replace(/\s*(?=↫)/g,"");
        笔记正文 = 笔记正文.replace(/(?<=^|↫)[\s\t]*([^\s\t#`\[\]\(\)↫]+[^\.\?\!:,0-9，：。？！）↫])(?=(↫|$))/mg,"↫# $1↫");
        //笔记正文 = 笔记正文.replace(/#+([^#↫]+)↫*$/mg,"$1");    //取消末行标题
        笔记正文 = 笔记正文.replace(/↫{3,}/g,"\r\n\r\n");
        笔记正文 = 笔记正文.replace(/↫/g,"\r\n");
        this.替换笔记正文 (笔记正文);
    };

    大字号文本() {    //修复 标题行首的空格或制表符影响正常格式 的问题。 20220807
		this.获取编辑器信息 ();
        if(所选文本 == ""){
            return;
        };

        let tagReg = /(.*)\<font size=5px\>([^\<\>]*)\<\/font\>(.*)/ig;	//是否包含html语法
	  	if (tagReg.test(所选文本)) {
            所选文本 = 所选文本.replace(tagReg, "$1$2$3");
		}else{
			所选文本 = "<font size=5px>" + 所选文本 + "</font>";
		}
        this.替换所选文本 (所选文本);     
    };

    指定当前文件名 () {
        this.获取编辑器信息 ();
        if (所选文本 == ""){return};
        当前文件路径 = this.app.workspace.getActiveFile().path;
        //navigator.clipboard.writeText(所选文本);
        //this.app.workspace.activeLeaf.replaceSelection(所选文本);
        this.app.vault.adapter.rename(当前文件路径, 所选文本+".md"); //强行重命名，不能全局更新
        console.log(当前文件路径+"\n"+所选文本);
    };

    获取相对路径 () {
        当前文件 = this.app.workspace.getActiveFile();
        当前文件路径 = 当前文件.path;
        //navigator.clipboard.writeText(当前文件路径)
        let 相对目录 = 当前文件路径.replace(/(?<=\/)[^\/]+$/m,"");
        new obsidian.Notice("当前笔记位于："+相对目录);
    };

    智能粘贴() {
    	this.获取编辑器信息 ();
    	let 分隔行 = "";        //获取 当前窗口  //其中const是【常数】
        navigator.clipboard.readText()
		.then(clipText => {
            let bgReg = /([^\t]+[\t]){3,}[^\t]+/;
            let urlReg = /^(https?:\/\/[^:]+)$/
            let PicUrlReg = /^(https?:\/\/[^:]+)\.(jpg|png|wemb|gif|mp3|mp4|wav|flv|mid)$/
            let pathReg = /^([c-z]:\\[^\/:\*\?\<\>\|]+)$/i
            let PicPathReg = /^([c-z]:\\[^\/:\*\?\<\>\|]+)\.(\w+)$/i
            let htmlReg = /^.*(\<\body[^\<\>]+\>.*\<\/body\>).*$/
            let tmpText = clipText.replace(/[\n ]/g,"");
            //console.log("获取到如下数据：\n"+ tmpText);
            if(urlReg.test(tmpText)){
                if(所选文本 ==""){
                    if(PicUrlReg.test(tmpText)){
                        clipText = clipText.replace(urlReg,"![图片]($1)");
                        new obsidian.Notice("剪贴板数据已转为MD嵌入语法！");
                    }else{
                        clipText = clipText.replace(urlReg,"[链接]($1)");
                        new obsidian.Notice("剪贴板数据已转为MD链接语法！");
                    }
                }else{
                    if(PicUrlReg.test(tmpText)){
                        clipText = clipText.replace(urlReg,"!["+所选文本+"]($1)");
                        new obsidian.Notice("剪贴板数据已附加到MD嵌入语法！");
                    }else{
                        clipText = clipText.replace(urlReg,"["+所选文本+"]($1)");
                        new obsidian.Notice("剪贴板数据已附加到MD链接语法！");
                    }
                }
            }else if(pathReg.test(tmpText)){
                if(PicPathReg.test(tmpText)){
                    clipText = clipText.replace(PicPathReg,"![本地]($1)");
                    new obsidian.Notice("剪贴板数据已转为本地文件嵌入式链接！");
                }else{
                    clipText = clipText.replace(pathReg,"[本地]($1)");
                    new obsidian.Notice("剪贴板数据已转为本地文件超链接！");
                }
                
            }else if(bgReg.test(tmpText)){
                clipText = clipText.replace(/\n/g,"■");
                clipText = clipText.replace(/\"([^■\|\"]+)■([^\|\n\"]+)\"/g,"$1<br>$2");
                分隔行 = clipText.replace(/■.*/g,"");
                分隔行 = 分隔行.replace(/\t/g,"|");
                分隔行 = 分隔行.replace(/([^\|]*)/g,":---");
                clipText = clipText.replace(/\t/g,"\|");
                clipText = clipText.replace(/^([^■]+)/,"$1■"+分隔行);
                clipText = clipText.replace(/■/g,"\n");
                clipText = clipText.replace(/^\|/mg,"　\|");
                clipText = clipText.replace(/\|$/mg,"\|　");
                clipText = clipText.replace(/^(?=[^\r\n])|(?<=[^\r\n])$/mg,"\|");
                clipText = clipText.replace(/(?<=\|)(?=\|)/g,"　");
                new obsidian.Notice("剪贴板数据已转为MD语法表格！");
            }else if(htmlReg.test(tmpText)){
                console.log('获取到富文本');
            }else/* if(codeReg.test(tmpText))*/{
                clipText = "```\n"+clipText+"\n```\n"
                new obsidian.Notice("剪贴板数据已转为代码块格式！");
            }
			编辑模式.replaceSelection(clipText);
            //在当前光标位置写入处理后的数据
		})
		.catch(err => {
			console.error('未能读取到剪贴板上的内容: ', err);
		});
    };

    图文粘贴(){
        navigator.clipboard.read().then((file_list) => {
            for(let item of file_list) {
              for(let file_type of item.types) {
                //console.log("文件类型", file_type)
                item.getType(file_type).then(res => {
                  if(["text/html"].includes(file_type)) {
                    res.text().then(text => {
                        //console.log(text);
                        navigator.clipboard.writeText(text);
                        text = text.replace(/<\/?(p|o:|v:|\!|a(\s|\s*\r*\n)*)[^<>]*>/ig,"");
                        text = text.replace(/(<span(\s|\s*\r\n)style='(color|background)[^<>]*>)([^<>]*)(<\/span>)/ig,"$1$4<■span>");
                        text = text.replace(/(<img[^<>]*>)([^<>]*)(<\/span>)/ig,"$1$2<■span>");
                        text = text.replace(/<span(\s|\s*\r\n)style='(font|mso)[^<>]*>/ig,"");
                        text = text.replace(/(&nbsp;)+(\r*\n)*(<\/span>)*/ig,"");
                        text = text.replace(/<\/?(u|i)>/ig,"");
                        text = text.replace(/<b(\s|\s*\r\n)style=[^<>]*>/ig,"<b>");
                        //console.log(text);
                        text = text.replace(/<(span)(\s|\s*\r\n)(style|lang|class)=[^<>]*>/ig,"");
                        text = text.replace(/<\/(span|h[1-6])>/ig,"");
                        text = text.replace(/(?<=<\/?(i))\s[^<>]*(?=>)/ig,"");
                        
                        text = text.replace(/<h1[^\<>]*>/ig,"# ");
                        text = text.replace(/<h2[^\<>]*>/ig,"## ");
                        text = text.replace(/<h3[^\<>]*>/ig,"### ");
                        text = text.replace(/<h4[^\<>]*>/ig,"#### ");
                        text = text.replace(/<h5[^\<>]*>/ig,"##### ");
                        text = text.replace(/<h6[^\<>]*>/ig,"###### ");
                        //console.log(text);

                        text = text.replace(/■/g,"");
                        //text = text.replace(/^[\t\s]+<\/?td>/mg,"|");
                        //text = text.replace(/<tr>/ig,"\r\n");
                        //text = text.replace(/\r\n\s*\r\n\s*<td>\s*\r\n\s*/ig,"|");
                        //text = text.replace(/<\/?t.*>/ig,"|");
                        text = text.replace(/<(tr|td)\s[^<>]*>/g,"<$1>");//table|
                        //text = text.replace(/\|\|+/g,"|");
                        //text = text.replace(/\|(\r*\n\s*)+\|/g,"|\n|");
                        text = text.replace(/&nbsp;\r*\n/ig,"");
                        text = text.replace(/(\r*\n\s*)+/g,"\n");
                        //let bg = text.match(/\|[^\n\|]*\|/);
                        /*if(bg){
                            let bt = bg[0];
                            let fgx = bt.replace(/(?<=\|)[^\|]*(?=\|)/g,"---");
                            text = text.replace(bt,bt+"\n"+fgx);
                        }*/
                        //console.log(text);
                        text = text.replace(/<img [^<>]*width=["']?(\d+)["']? height=["']?(\d+)["']? (\s*\r*\n)*src=["']?([^"']+)["']?[^>]+>[^<>]*<\/?span>/ig,"![img-$1-$2]($4)");
                        //text = text.replace(/(<b>|<\/b>)/g,"**");
                        text = text.replace(/^\s*\*\*?\s*\*\*?\s*$/mg,"");
                        text = text.replace(/(\r*\n[\s\t]*){2,}/g,"\n\n");
                        编辑模式.replaceSelection(text);
                        //在当前光标位置写入处理后的数据
                    }, (error) => { console.log(error) })
                  } else {
                      //console.log("无合适数据！")
                  }
                }, (error) => { console.log(error) })
              }
            }
          }, (error) => { console.log(error) })
    }

    计算所选结果() {
        this.获取编辑器信息 ();
        if(/^[\(\d\+\-][\(\)\d\+\-\/\*x\.]+[\d\)]$/i.test(所选文本)){
            if(/[x\*\/][x\*\/]+/i.test(所选文本)){
                new obsidian.Notice("这个题目，俺看不懂！");
                return;
            }
            let 结果 = eval(所选文本.replace(/x/ig,"*"));
            new obsidian.Notice(所选文本+"="+结果+"\n结果已写入剪贴板！");
            navigator.clipboard.writeText(结果);
        }else{
            new obsidian.Notice("数了数，您划选了"+所选文本.length+"个字符！");
        }
    }

    获取搜索结果() {
        var _a;
        var _linkTxt = "";
        const searchView = (_a = this.app.workspace.getLeavesOfType('search')[0]) === null || _a === void 0 ? void 0 : _a.view;
        if (!searchView) {
            new obsidian.Notice("请先执行搜索操作...");
            return;
        }
        const searchResults = searchView.dom.getFiles();
        if (!searchResults.length) {
            return;
        }
        const markdownFiles = searchResults.filter((file) => file.extension === 'md');
        if (!markdownFiles.length) {
            return;
        }
        for(var i= 0;i<markdownFiles.length;i++){
            _linkTxt +=  "[["+markdownFiles[i].basename+"]]\n"
        }
        navigator.clipboard.writeText(_linkTxt);
        new obsidian.Notice("搜索结果已写入剪贴板！\n请执行粘贴操作...");
    };
    
    获取标注文本() {
        this.获取编辑器信息 ();
        if (!笔记正文) return;
        let tmp = 笔记正文.replace(/^(?!#+ |#注释|#标注|#批注|#反思|#备注|.*==|.*%%).*$|^[^#\n%=]*(==|%%)|(==|%%)[^\n%=]*$|(==|%%)[^\n%=]*(==|%%)/mg,"\n");
        tmp = tmp.replace(/[\r\n|\n]+/g,"\n")        
        new obsidian.Notice("已成功获取标注类文本，可以粘贴！");
        navigator.clipboard.writeText(tmp);
    };

    获取无语法文本() {
        this.获取编辑器信息 ();
        if(所选文本 == ""){
            new obsidian.Notice("请先划选部分文本，再执行命令！");
        }else{
            let mdText = /(^#+\s|(?<=^|\s*)#|^>|^\- \[( |x)\]|^\+ |<[^<>]+>|^1\. |^\-+$|^\*+$|==|\*+|~~|```|!*\[\[|\]\])/mg;
            所选文本 = 所选文本.replace(/\[([^\[\]]*)\]\([^\(\)]+\)/img,"$1");
            所选文本 = 所选文本.replace(mdText,"");
            所选文本 = 所选文本.replace(/^[ ]+|[ ]+$/mg,"");
            所选文本 = 所选文本.replace(/(\r\n|\n)+/mg,"\n");
            new obsidian.Notice("已成功获取无语法文本，可以粘贴！");
            navigator.clipboard.writeText(所选文本);
        }
    };

    获取当前字数() {
        this.获取编辑器信息 ();
        if (!笔记正文) return;
        let showTip = "";
        let showTip1 = "";
        let showTip2 = "";

        let 可见字符 = 笔记正文.match(/[^\s\t\r\n]/g);
        if(可见字符){
            showTip1 += "\n可见字符 "+可见字符.length;

            let 汉字个数 = 笔记正文.match(/[一-龥]/g);
            if(汉字个数){
                showTip1 += "\n- 汉字 "+汉字个数.length;
            }

            let 字母个数 = 笔记正文.match(/[a-z]/ig);
            if(字母个数){
                showTip1 += "\n- 字母 "+字母个数.length;
            }

            let 数字个数 = 笔记正文.match(/\d/ig);
            if(数字个数){
                showTip1 += "\n- 数字 "+数字个数.length;
            }

            let 标点个数 = 笔记正文.match(/[,，\.。\\、\/\?？\!！:：;；—【】（）{}《》#&@\$\^“”‘’'"\]\[\(\)—…]/ig);
            if(标点个数){
                showTip1 += "\n- 标点 "+标点个数.length;
            }
        }else{
            showTip1 = "\n0 个可见字符";
        }

        let 不可见字符 = 笔记正文.match(/[\s\t\r\n]/g);
        if(不可见字符){
            showTip2 += "\n不可见字符 "+不可见字符.length;

            let 空格个数 = 笔记正文.match(/[ 　\t]/g);
            if(空格个数){
                showTip2 += "\n- 空格 "+空格个数.length;
            }

            let 换行个数 = 笔记正文.match(/\r*\n/g);
            if(换行个数){
                showTip2 += "\n- 换行 "+换行个数.length;
            }
        }else{
            showTip2 = "\n0 个不可见字符";
        }
        
         new obsidian.Notice("当前笔记含有:"+showTip1+"\n"+showTip2,0);
    };

    嵌入当前网址页面 () {
        this.获取编辑器信息 (); 
        let vid,web;
        let 基本格式 = '\n<iframe src="■" width=100% height="500px" frameborder="0" scrolling="auto"></iframe>';
        if(所选文本.match(/^https?:\/\/[^:]+/)){
            if(所选文本.match(/^https?:\/\/v\.qq\.com/)){
                vid = 所选文本.replace(/^http.*\/([^\/=\?\.]+)(\.html.*)?$/,"$1");
                web = "https://v.qq.com/txp/iframe/player.html?vid="+vid;
            }else if(所选文本.match(/^https?:\/\/www\.bilibili\.com/)){
                vid = 所选文本.replace(/^http.*\/([^\/=\?\.]+)(\?spm.*)?$/,"$1");
                web = "https://player.bilibili.com/player.html?bvid="+vid;
            }else if(所选文本.match(/^https?:\/\/www\.youtube\.com/)){
                vid = 所选文本.replace(/^http.*?v=([^\/=\?\.]+)(\/.*)?$/,"$1");
                web = "https://www.youtube.com/embed/"+vid;
            }else{
                web = 所选文本;
            }
            基本格式 = 基本格式.replace(/■/,web);
            笔记全文.replaceRange(基本格式, {line:当前行号,ch:当前行文本.length},{line:当前行号,ch:当前行文本.length});
            编辑模式.exec("goRight");
        }else{
            new obsidian.Notice("所选文本不符合网址格式，无法嵌入！");
        }
    };

    列表转为图示 () {
        this.获取编辑器信息 (); 
        let 大纲文本 = 所选文本.replace(/(    |\t)/mg,"■");
        大纲文本 = 大纲文本.replace(/(\-\s|\d+\.\s)/mg,"");   //对所有文本行的行首进行替换整理,去除-
        大纲文本 = 大纲文本.replace(/\s+$/mg,"");   //对所有文本行的行尾进行替换去除
        大纲文本 = 大纲文本.replace(/\n/g,"↵");
        大纲文本 = 大纲文本.replace(/↵+$/,"");   //去除末尾多余换行符
        let tagAry = 大纲文本.split("↵");
        //new obsidian.Notice(tagAry[0]);
        let fName = "";
        let 主要语法 = "";
        for(var i =0;i<tagAry.length;i++){
            var thisLine = tagAry[i];   //此行文本
            var n = thisLine.lastIndexOf("■");
            if(i>0){
                var upLine = tagAry[i-1];   //上行文本
                var m = upLine.lastIndexOf("■");
            }
            
            if(n<0){//无■，即为根级大纲,可创建@导航页面
                fName = thisLine;
            }else{
                //new obsidian.Notice(upLine+"  "+m+"\n"+thisLine+"  "+n);
                //比较下行与当前行的■数，三种情况，下行多，两行同，下行少
                thisLine = thisLine.replace(/^■+/,"");  //去除标识符号
                if(n>m){
                    //本行多，追加-当前行，前缀@
                    fName = fName+"-->"+thisLine;
                }else if(n==m){
                    //替换末尾-旧名称 为 -当前行
                    fName = fName.replace(/(?<=(^|\-\-\>))[^\-\>]+$/,thisLine);
                }else{
                    var cha=Number(m-n)+1;  //计算上行、本行的■数
                    fName = fName.replace(eval("/(\-\-\>[^\-\>]+){"+cha+"}$/"),"-->"+thisLine);
                }
                var 行语法 = fName.replace(/^.*\-\-\>(?=[^\-\>]+\-\-\>[^\-\>]+$)/mg,"");
                主要语法 = 主要语法 + "↵"+ 行语法;
            }
        }
        var 输出语法 = "%%此图示由列表文本转换而成！%%↵"+主要语法
        //编辑模式.setCursor({line:0,ch:0});
        //笔记正文 = this.获取笔记正文();
        编辑模式.exec("goRight");
        编辑模式.exec("goDown");
        this.获取编辑器信息 ();
        输出语法 = 输出语法.replace(/↵/g,"\n");
        笔记全文.replaceRange("```mermaid\ngraph TD\n"+输出语法+"\n```\n", 当前光标, 当前光标);

        /*
        var 新正文 = 笔记正文.replace(/\n/g,"↵");
        if(新正文.includes("%%此图示由列表文本转换而成！%%")){
            新正文 = 新正文.replace(/%%此图示由列表文本转换而成！%%↵.+?(?=↵```)/g, 输出语法);
            新正文 = 新正文.replace(/↵/g,"\n");
            this.替换笔记正文 (新正文);
        }else{
            new obsidian.Notice("列表文本已转为MerMaid语法。\n可以粘贴！");
            输出语法 = 输出语法.replace(/↵/g,"\n");
            navigator.clipboard.writeText("```mermaid\ngraph TD\n"+输出语法+"\n```\n");
        };
        */
    }

    升序排列所选段落() {
		this.获取编辑器信息 ();
        if (所选文本 == "") return;
        var 段落 = 所选文本.replace(/\n\s*(?=\n)/g,"").split("\n");    //去除空行，拆分为数组
        段落.sort();
        this.替换所选文本 (段落.join("\n"));
    };
    降序排列所选段落() {
		this.获取编辑器信息 ();
        if (所选文本 == "") return;
        var 段落 = 所选文本.replace(/\n\s*(?=\n)/g,"").split("\n");    //去除空行，拆分为数组
        段落.sort().reverse();
        this.替换所选文本 (段落.join("\n"));
    };

    /*
    多行引用文本() {
		this.获取编辑器信息 ();
        var 所选数据 = 编辑模式.listSelections();
        if(所选数据){
            var 首选行号 = 所选数据[0].anchor.line;
            var 末选行号 = 所选数据[0].head.line;
            for ( var i = 首选行号;i <= 末选行号;i++){
                var 当前行文本 = 编辑模式.getLine(i);
                if(当前行文本.startsWith(">")){
                    笔记全文.replaceRange("", {line:i,ch:0},{line:i,ch:1});
                }else{
                    笔记全文.replaceRange(">", {line:i,ch:0},{line:i,ch:0});
                }
            }
        }else{
            return;
        }
    };
    */

    添加段落编号() {
		this.获取编辑器信息 ();
        if (所选文本 == ""){
            所选文本 = 笔记正文;
        };
        let 排除行 = /^(#+.*|```|\.*|\|([^\|]*\|)*|\>.*)\s*$/m;   //排除特殊行文本
        //new obsidian.Notice("当前为标题行 "+_str);
        var 末行行号 = 编辑模式.lastLine();
        let lineId = 1;
        for(var i= 0;i<=末行行号;i++){
            var 本行文本 = 编辑模式.getLine(i);
            if(排除行.test(本行文本)){
               
            }else{
                笔记全文.replaceRange(lineId+". ", {line:i,ch:0},{line:i,ch:0});
                lineId ++;
            }
        }
    };

    去除段落编号() {
		this.获取编辑器信息 ();
        if (!笔记正文) return;
        编辑模式.exec("goStart");
        笔记正文 = 笔记正文.replace(/^[0-9]+\.\s+([^\s])/mg,"$1");
        this.替换笔记正文 (笔记正文);
    };


    折叠同级标题() {
		this.获取编辑器信息 ();
        if (!笔记全文) return;
        if(/^#+ /.test(当前行文本)){
            this.app.commands.executeCommandById('editor:unfold-all');
            let _str = 当前行文本.replace(/^(#+) .*$/,"$1");   //获取前面的多个#号
            //new obsidian.Notice("当前为标题行 "+_str);
            var 末行行号 = 编辑模式.lastLine();
            var arr = 编辑模式.getRange({line:0,ch:0},{line:末行行号,ch:0}).split("\n");
            //console.log(arr);
            for (var i=arr.length; i>=0; i--) {
                if(eval("/^"+_str+"(?=[^#])/").test(arr[i])) {
                    编辑模式.setCursor({line:i,ch:0});
                    this.app.commands.executeCommandById('editor:toggle-fold');
                }
            }
        }
    };

    调高所有标题级别() {
		this.获取编辑器信息 ();
        if (!笔记正文) return;
        笔记正文 = 笔记正文.replace(/(?<=^#+)# /mg," ");
        this.替换笔记正文 (笔记正文);
    };

    调低所有标题级别() {
		this.获取编辑器信息 ();
        if (!笔记正文) return;
        笔记正文 = 笔记正文.replace(/(?<=^#*)# /mg,"## ");
        this.替换笔记正文 (笔记正文);
    };
    
    /*
    以下修改所选范围的标题行级别，代码失败，不能很好的获取到选区的首行和末行，处理后无法恢复选区范围
    调高所选标题级别() {
		this.获取编辑器信息 ();
        if (!所选文本) return;
        所选文本 = 所选文本.replace(/(?<=^#+)# /mg," ");
        this.替换所选文本 (所选文本);
    };

    调低所有标题级别() {
		this.获取编辑器信息 ();
        if (!所选文本) return;
        所选文本 = 所选文本.replace(/(?<=^#*)# /mg,"## ");
        this.替换所选文本 (所选文本);
    };
    */
    
    //火冷添加

    当前行内容根据光标所在内部链接分割() {
        this.获取编辑器信息()
        var 行内容根据光标内部链接分割 = [];
        //console.log(选至行首);
        //console.log(选至行尾);
        if (选至行首.endsWith("]]")) { //光标在内部链接范围(含]]后面，因为添加内部链接光标默认在此位置)
            var m0 = 选至行首.match(/^(.*)(!?\[\[.*?\]\])$/);
            行内容根据光标内部链接分割 = [m0[1], m0[2], 选至行尾];
        } else if (选至行首.match(/\[\[/) && 选至行尾.match(/\]\]/)) { //光标在内部链接内(TODO 不支持在]]符号内)
            var m0 = 选至行首.match(/^(.*)(!?\[\[.*)$/);
            var m1 = 选至行尾.match(/^(.*?\]\])(.*)/);
            行内容根据光标内部链接分割 = [m0[1], m0[2]+m1[1], m1[2]];
        } else {
            行内容根据光标内部链接分割 = ["not in [[]]"];
        }
        //console.log(行内容根据光标内部链接分割);
        return 行内容根据光标内部链接分割;
    }

    //["![[", "内部链接文本" , "]]"]
    内部链接分割(内部链接含符号) {
        //console.log("内部链接含符号 = " + 内部链接含符号);
        var m = 内部链接含符号.match(/^(!?\[\[)(.*)(\]\])$/);
        if (m)
            return [m[1],m[2],m[3]];
        else
            console.log("内部链接分割 失败");
    }

    内部链接名称(内部链接文本) {
        return 内部链接文本.replace(/.*\//, '');
    }

    //添加内部链接内容的别名，比如 a/b/c → a/b/c|c
    修改内部链接的显示名称() {
        var arr = this.当前行内容根据光标所在内部链接分割();
        if (arr[1].indexOf("|") > -1) {
            return;
        }
        var 内部链接数组 = this.内部链接分割(arr[1]);
        var 名称 = this.内部链接名称(内部链接数组[1]);
        if (名称.indexOf("#") > -1) //有标题，取标题后面
            名称 = 名称.replace(/.*#/, '');
        if (名称.indexOf("^") > -1) //有段落标记，则删除
            名称 = 名称.replace(/\s+\^.*/, '');
        内部链接数组[1] += `|${名称}`;
        //console.log(内部链接数组);
        arr[1] = 内部链接数组.join("");
        //console.log(arr);
        this.app.workspace.activeLeaf.view.editor.setLine(当前行号, arr.join(""));
        编辑模式.setCursor({line:当前行号,ch:arr[0].length+arr[1].length-2}); //-2是因为
    }

    所有上级标题路径() {
        编辑模式 = this.获取编辑模式 ();
        当前光标 = 编辑模式.getCursor();
        当前行号 = 当前光标.line;
        var arr = 编辑模式.getRange({line:0,ch:0},{line:当前行号,ch:0}).split("\n");
        //console.log(arr);
        //第一个要匹配的 re
        if (arr[arr.length-1].match(/^(\#+)\s/)) { //当前是标题，则查找上一级标题
            var l = arr[arr.length-1].match(/^(\#+)\s/)[1].length - 1;
            if (l == 0)
                return [];
            var re = new RegExp(`^\#{${l}}\\s`);
        } else {
            var re = /^\#+\s/;
        }
        //console.log(re);
        var arrRes = [];
        for (var i=arr.length-1; i>=0; i--) {
            //console.log(arr[i]);
            if (re.test(arr[i])) {
                arrRes.push(arr[i]);
                var l = arr[i].match(/^(\#+)\s/)[1].length - 1;
                if (l == 0)
                    return arrRes;
                re = new RegExp(`^\#{${l}}\\s`);
                //console.log(re);
            }
        }
        return arrRes;
    };

    折叠某级别标题(折叠等级) {
        this.获取编辑器信息 ();
        if (!笔记全文) return;
        this.app.commands.executeCommandById('editor:unfold-all');
        var 末行行号 = 编辑模式.lastLine();
        var arr = 编辑模式.getRange({line:0,ch:0},{line:末行行号,ch:0}).split("\n");
        var re = new RegExp(`^\#{${折叠等级}}\\s`);
        for (var i=arr.length; i>=0; i--) {
            if (re.test(arr[i])) {
                编辑模式.setCursor({line:i,ch:0});
                this.app.commands.executeCommandById('editor:toggle-fold');
            }
        }
    };

    插入有效空行() {
		this.获取编辑器信息 ();
        if (!笔记全文) return;
        笔记全文.replaceRange("　\n", 当前光标, 当前光标);
        编辑模式.exec("goRight");
        编辑模式.exec("goRight");
    };

    批量插入空行() {
		this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = 笔记正文.replace(/(?<!^(\s*\- |\s*[0-9]+\.|\s*\>|\n)[^\n]*)\n(?!(\s*\- |\s*[0-9]+\.|\s*\>|\n))/g,"$1\n\n");
        this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = 所选文本.replace(/(?<!^(\s*\- |\s*[0-9]+\.|\s*\>|\n)[^\n]*)\n(?!(\s*\- |\s*[0-9]+\.|\s*\>|\n))/g,"$1\n\n");
            this.替换所选文本 (所选文本);
        };        
    };
    
    批量去除空行() {
        this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = 笔记正文.replace(/(^\s*\n|\r\n|\n)[\t\s]*(\r\n|\n)/g,"\n");
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = 所选文本.replace(/(\r\n|\n)[\t\s]*(\r\n|\n)/g,"\n");
            this.替换所选文本 (所选文本);
        };        
    };

    空格转为空行() {
        this.获取编辑器信息 ();
        if (所选文本 == ""){
            笔记正文 = 笔记正文.replace(/(?<=[一-龥。？！])[\t\s](?=[一-龥])/g,"\n");
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = 所选文本.replace(/(?<=[一-龥。？！])[\t\s](?=[一-龥])/g,"\n");
            this.替换所选文本 (所选文本);
        };        
    };

    全文首行缩进() {
		this.获取编辑器信息 ();
        编辑模式.exec("goStart");

        if (!笔记正文) return;
        //new obsidian.Notice(笔记正文);
        if(isIndent){
            笔记正文 = 笔记正文.replace(/^[‌‌‌‌　]+/mg,"");
            笔记正文 = 笔记正文.replace(/^(?!(\s*\d+\.\s|\s*\-\.\s|[\n\s\>#]+|```|\-\-\-|\|[^\|]|\*\*\*))/mg,"‌‌‌‌　　");
            isIndent = false;
        }else{
            笔记正文 = 笔记正文.replace(/^[‌‌‌‌　]+/mg,"");
            isIndent = true;
        }
        this.替换笔记正文 (笔记正文);
    };

    当前行缩进() {
		this.获取编辑器信息 ();
        if (!笔记全文) return;
        let 新文本 = "";
        let 已缩进 = 当前行文本.includes("‌　　");
        let 偏移 = 当前光标.ch;
        新文本 = 当前行文本.replace(/^[‌‌‌‌　]+/mg,"");
        if(已缩进){
        }else{
            新文本 = 新文本.replace(/^(?!(\s*\d+\.\s|\s*\-\.\s|[\n\s\>#]+|```|\-\-\-|\|[^\|]|\*\*\*))/mg,"‌‌‌‌　　");
        }
        笔记全文.replaceRange(新文本,  {line:当前行号,ch:0}, {line:当前行号,ch:当前行文本.length});
        if(已缩进){
            if(当前光标.ch>3){
                编辑模式.setCursor({line:当前行号,ch:Number(偏移-5)});
            }else{
                编辑模式.setCursor({line:当前行号,ch:0});
            }
        }else{
            编辑模式.setCursor({line:当前行号,ch:Number(偏移+5)});
        }
    };

    行首添加空格() {
		this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = 笔记正文.replace(/(?<=(^|\n))(?!(\-\-\-|\*\*\*|\s))/g,"  ");
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = 所选文本.replace(/(?<=(^|\n))(?!(\-\-\-|\*\*\*|\s))/g,"  ");
            this.替换所选文本 (所选文本);
        };        
    };

    去除行首空格() {
        this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = 笔记正文.replace(/(?<=(^|\n))\s+/g,"");
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = 所选文本.replace(/(?<=(^|\n))\s+/g,"");
            this.替换所选文本 (所选文本);
        };        
    };

    末尾追加空格() {
		this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = 笔记正文.replace(/(?<!(\-\-\-|\*\*\*|\s\s))\n/g,"  \n");
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = 所选文本.replace(/(?<!(\-\-\-|\*\*\*|\s\s))\n/g,"  \n");
            this.替换所选文本 (所选文本);
        };        
    };
        
    去除末尾空格() {
        this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = 笔记正文.replace(/\s+(?=($|\n))/g,"")
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = 所选文本.replace(/\s+(?=($|\n))/g,"")
            this.替换所选文本 (所选文本);
        };        
    };

    上方插入空行(_str) {
        this.获取编辑器信息 ();
        if (!笔记全文) return;
        var 新文本 = "\r\n"+当前行文本;
        笔记全文.replaceRange(新文本, {line:当前行号,ch:0},{line:当前行号,ch:当前行文本.length});
    };

    下方插入空行(_str) {
        this.获取编辑器信息 ();
        if (!笔记全文) return;
        var 新文本 = 当前行文本+"\r\n";  //.replace(/^([^\r\n]*)$/,"$1\n");
        笔记全文.replaceRange(新文本, {line:当前行号,ch:0},{line:当前行号,ch:当前行文本.length});
        编辑模式.setSelection({line:当前行号,ch:当前行文本.length+1}, {line:当前行号,ch:当前行文本.length+1});
    };
   
    添加间隔空格() {
        this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = 笔记正文.replace(/([a-zA-Z]+)([一-鿆]+)/g,"$1 $2")
            笔记正文 = 笔记正文.replace(/([一-鿆]+)([a-zA-Z]+)/g,"$1 $2")
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = 所选文本.replace(/([a-zA-Z]+)([一-鿆]+)/g,"$1 $2")
            所选文本 = 所选文本.replace(/([一-鿆]+)([a-zA-Z]+)/g,"$1 $2")
            this.替换所选文本 (所选文本);
        };        
    };

    去除所有空格() {
        this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = 笔记正文.replace(/[ 　]+/g,"")
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = 所选文本.replace(/[ 　]+/g,"")
            this.替换所选文本 (所选文本);
        };        
    };

    去除所有注释() {
        this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = 笔记正文.replace(/%%[^%\n]*%%/g,"")
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = 所选文本.replace(/%%[^%\n]*%%/g,"")
            this.替换所选文本 (所选文本);
        };        
    };

    /* 此功能暂未启用。*/
     续选当前文本() { 
        this.获取编辑器信息 ();
        var lang = 选至行尾.indexOf(所选文本);
        var 起始 = 选至行首.length+lang;
        var 结束 = 起始 + 所选文本.length;
        if(lang<0){ return};
        编辑模式.setSelection({line:当前行号,ch:起始}, {line:当前行号,ch:结束});
    };

    搜索当前文本() {
        var 当前文件 = this.app.workspace.getActiveFile();
        var 当前文件路径 = 当前文件.name;
        //new obsidian.Notice(当前文件路径);
        当前文件路径 = 当前文件路径.replace(/\.md$/,"")
        var view = this.app.workspace.getActiveViewOfType(obsidian.MarkdownView);
        var _txt = view.getSelection();
        _txt = _txt.replace(/^/,"path:"+当前文件路径+" /")
        this.app.internalPlugins.getPluginById('global-search').instance.openGlobalSearch(_txt)
    };

    修复外来文本(){
        this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = this.修替断行(笔记正文);
            笔记正文 = this.修替标点(笔记正文);
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = this.修替断行(所选文本);
            所选文本 = this.修替标点(所选文本);
            this.替换所选文本 (所选文本);
        };  
    }

    修复意外断行() {
        this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = this.修替断行(笔记正文);
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = this.修替断行(所选文本);
            this.替换所选文本 (所选文本);
        };        
    };

    修复错误语法() {
        this.获取编辑器信息 ();
        所选文本 = 所选文本.replace(/[\[【]([^\[\]【】]*)[】\]][（|\(]([^\(\)（）]*)[）|\)]/g,"\[$1\]\($2\)");
        //将 【】（）或【】() 转换为[]()
        所选文本 = 所选文本.replace(/\[+([^\[\]]*)\]+\(/g,"\[$1\](");
        //处理 bookXnote 回链语法，将 [[链接]]() 转换为 []()
        所选文本 = 所选文本.replace(/(?<=^|\s)    /mg,"\t");
        //把 四个空格转换为 制表符
        所选文本 = 所选文本.replace(/(?<=\]\([^\(\)]+\))$/g,"  ");
        //在超链接末尾处补加两空格
        所选文本 = 所选文本.replace(/\*\s+\>\s+/g,"- ");
        //处理 bookXnote 回链语法中的列表
        所选文本 = 所选文本.replace(/(?<=\s)[0-9]+。 /g,"1. ");
        //把 1。 转换为 有序列表样式
        if (所选文本 == "") {
            this.替换笔记正文 (所选文本);
        }else{            
            this.替换所选文本 (所选文本);
        };        
    };

    修复错误标点() {
        this.获取编辑器信息 ();
        if (所选文本 == "") {
            笔记正文 = this.修替标点(笔记正文);
            this.替换笔记正文 (笔记正文);
        }else{
            所选文本 = this.修替标点(所选文本);
            this.替换所选文本 (所选文本);
        };        
    };

    修替断行(_str){
        _str = _str.replace(/(?<=[^a-zA-Z])\s+(?=\r*\n)/g,"")
        _str = _str.replace(/(?<=\w)\s+(?=\r*\n)/g," ")
        //去除末尾非字母后的空格，将末尾字母后的多个空格保留一个
        _str = _str.replace(/([^。？！\.\?\!\w])(\r*\n)+/mg,"$1")
        _str = _str.replace(/(\w)(\r*\n)+/mg,"$1 ")
        return _str;
    }

    修替标点(_str){
        _str = _str.replace(/\n/g,"↫");
        _str = _str.replace(/(?<=[一-龥”’）》】])\s*,\s*([一-龥“‘（《【↫])/g,"，$1");
        _str = _str.replace(/(?<=[一-龥”’）》】])\s*\.\s*([一-龥“‘（《【↫]|$)/g,"。$1");   //数字后的黑点不转为句号
        _str = _str.replace(/(?<=[一-龥”’）》】])\s*\?\s*([一-龥“‘（《【↫]|$)/g,"？$1");
        _str = _str.replace(/(?<=[一-龥”’）》】])\s*:\s*([一-龥“‘（《【↫]|$)/g,"：$1");
        _str = _str.replace(/(?<=[一-龥”’）》】])\s*;\s*([一-龥“‘（《【↫]|$)/g,"；$1");
        _str = _str.replace(/(?<=[a-zA-Z0-9])\s*;\s*([一-龥“‘（《【↫]|$)/g,"；$1");   //数字、字母与中文之间的分号转为中文分号
        _str = _str.replace(/(?<=[一-龥”’）》】])\s*;\s*([a-zA-Z0-9↫]|$)/g,"；$1");   //中文与数字、字母之间的分号转为中文分号
        _str = _str.replace(/(?<=[a-zA-Z0-9])\s*;\s*([一-龥↫]|$)/g,"；$1");   //数字、字母与中文之间的分号转为中文分号
        _str = _str.replace(/(?<=[一-龥])\s*;\s*([a-zA-Z0-9↫]|$)/g,"；$1");   //中文与数字、字母之间的分号转为中文分号
        _str = _str.replace(/(?<=[一-龥])\s+(?=[一-龥])/g,"");
        _str = _str.replace(/(?<=[^一-龥])，([^一-龥])/g,",$1");
        _str = _str.replace(/(?<=[^一-龥])。([^一-龥])/g,".$1");   //数字后的句号不转为黑点
        _str = _str.replace(/(?<=[^一-龥])？([^一-龥])/g,"?$1");
        _str = _str.replace(/(?<=[^一-龥])：([^一-龥])/g,":$1");
        _str = _str.replace(/(?<=[^一-龥])；([^一-龥])/g,";$1");
        _str = _str.replace(/[\(（)]([^一-龥]+)[\)）]/g,"($1)");
        _str = _str.replace(/[\(（)](.*?[一-龥].*?)[\)）]/g,"（$1）");
        _str = _str.replace(/：：/g,"::");
        _str = _str.replace(/【【/g,"[[");
        _str = _str.replace(/】】/g,"]]");
        _str = _str.replace(/(?<=[,\.?!])([^\s0-9a-zA-Z])/g," $1");
        _str = _str.replace(/↫/g,"\n");
        return _str;
    }


    转换路径() {
        this.获取编辑器信息 ();
        if(所选文本 == "" || 笔记正文==null){return};
        var link1 = /^[a-zA-Z]:\\/;	//符合普通路径格式
        var link2 = /^(\[[^\[\]]*\]\()*file:\/\/\/[^\(\)]*\)*/;	//符合[](file路径)格式
        var link3 = /^\[[^\[\]]*\]\(([a-zA-Z]:\\[^\(\)]*)\)*/;	//意外路径格式
        if (link1.test(所选文本)){
            所选文本 = 所选文本.replace(/\s/mg,"%20");
            所选文本 = 所选文本.replace(/^(.*)$/m,"\[file\]\(file:///$1\)");
            所选文本 = 所选文本.replace(/\\/img,"\/");
            this.替换所选文本 (所选文本);
        }else if(link2.test(所选文本)){
            所选文本 = 所选文本.replace(/%20/mg," ");
            所选文本 = 所选文本.replace(/^(\[[^\[\]]*\]\()*file:\/\/\/([^\(\)]*)\)*/m,"$2");
            所选文本 = 所选文本.replace(/\//mg,"\\");
            this.替换所选文本 (所选文本);
        }else if(link3.test(所选文本)){
            所选文本 = 所选文本.replace(/^\[[^\[\]]*\]\(([a-zA-Z]:\\[^\(\)]*)\)*/m,"$1");
            this.替换所选文本 (所选文本);
        }else{
            new obsidian.Notice("您划选的路径格式不正确！");
            return;
        }
    };

    拆分多行() {
        this.获取编辑器信息 ();
        if(所选文本 == "" || 笔记正文 == null){return};
        所选文本 = 所选文本.replace(/([。？！]) /g,"$1\n");
        this.替换所选文本 (所选文本);
    };

    简体转繁() {
        this.获取编辑器信息 ();
        if (所选文本 == "") return;
        for (var i=0;i<简体字表.length;i++){ 
            所选文本 = 所选文本.replace(eval("/"+简体字表[i]+"/g"),繁体字表[i]);
        }
        this.替换所选文本 (所选文本);
    }

    繁体转简() {
        this.获取编辑器信息 ();
        if (所选文本 == "") return;
        for (var i=0;i<繁体字表.length;i++){ 
            所选文本 = 所选文本.replace(eval("/"+繁体字表[i]+"/g"),简体字表[i]);
        }
        this.替换所选文本 (所选文本);
    }

    生成时间戳() {
        var date = new Date();
        return date.getFullYear().toString() + this.pad2(date.getMonth() + 1) + this.pad2(date.getDate()); // + this.pad2(date.getHours()) + this.pad2(date.getMinutes()) + this.pad2(date.getSeconds()
    };
    pad2(n) {
        return n < 10 ? '0' + n : n
    };
}


class editSettingsTab extends obsidian.PluginSettingTab {
    constructor(app, plugin) {
        super(app, plugin);
        this.plugin = plugin;
    }

    display() {
        var plugin = this.plugin;
        var containerEl = this.containerEl;
        containerEl.empty();
        containerEl.createEl('h2', { text: "ZH 增强编辑 V"+当前版本 });
        new obsidian.Setting(containerEl)
            .setName("📣 转换内部链接「Alt+Z」：在选文两端添加或去除 [[ ]] 符号")
            .setDesc("支持转换多行文本（需用换行符分隔）或多句文本（需用顿号分隔）。")

        new obsidian.Setting(containerEl)
            .setName("📣 转换潜在链接「待设置」：判断当前笔记的正文部分，将符合的文本转为内部链接")
            .setDesc("此功能不处理 Yaml 和标签内容，只对笔记的正文部分进行处理。感谢 平果（184537266）提供建议。")
        new obsidian.Setting(containerEl)
            .setName("- 特定标题列表")
            .setDesc("优先处理匹配右侧列表的文本，如右表为空，才按库标题名称进行判断并转换。建议提取全库笔记标题进行整理并放入此处。")
            .addTextArea((text) => {text.setPlaceholder("一行文字一则标题\n不要使用禁止符号")
            .setValue(this.plugin.settings.linkWords).onChange((value) => {
                    this.plugin.settings.linkWords = value;
                    this.plugin.saveSettings();
                });
            text.inputEl.rows = 20;
            text.inputEl.cols = 60;
            });

        var div0 = containerEl.createEl('p', {
            cls: 'recent-files-donation',
        });

        var linkText = document.createDocumentFragment();
        linkText.appendText("📣 转换同义链接「Alt+Q」：将选文转换为 [[|选文]] 样式后再选择文档")
        linkText.appendChild(document.createElement('br'));
        linkText.appendText("📣 转换标签「Alt+Shift+3」：将选文转换为 #选文 标签样式或反向转换")
        linkText.appendChild(document.createElement('br'));
        div0.appendChild(linkText);

        /*
        new obsidian.Setting(containerEl)
            .setName('📣 智能换行「Enter」 默认支持```代码块```内换行缩进效果')
            .setDesc('启用此项后，在非列表或代码块的文本中按下回车后补加一次换行；如想普通换行，可按下 Shift+Enter 键。')
            .addToggle(toggle => toggle.setValue(this.plugin.settings.twoEnter)
            .onChange((value) => {
            this.plugin.settings.twoEnter = value;
            this.plugin.saveSettings();
        }));

        new obsidian.Setting(containerEl)
            .setName("📣 插入制表符「Tab」 在普通文本行中插入制表符效果")
            .setDesc("启用此项后，在普通文本行中按下 Tab 键会插入4个空格，不再整行缩进。")
            .addToggle(toggle => toggle.setValue(this.plugin.settings.isTab)
            .onChange((value) => {
            this.plugin.settings.isTab = value;
            this.plugin.saveSettings();
        }));
        */


        new obsidian.Setting(containerEl)
            .setName("📣 智能语法「Alt+;」：自动转换、匹配或跳过各种类型的括号或代码块语法")
            .setDesc("可将[( (< ([ \"[ \'[等组合转为〖〈〔『「，或将dv qy mm CSS js ty等字符串为代码块，将类型词语转为Callout引用语法。")

        new obsidian.Setting(containerEl)
            .setName("📣 智能粘贴「Ctrl+Alt+V」：将复制的内容粘贴为Md语法样式")
            .setDesc("依据复制内容的类型，将表格、网址、本地路径或代码直接粘贴为MD表格、超链接或代码块格式。")

        new obsidian.Setting(containerEl)
            .setName("📣 键控光标移动「Alt+I, +J, +K, +L」")
            .setDesc("按下Alt +I向上 +J向左 +K向下 +L向右 +U文首 +N文末 快捷键，控制光标移动位置。")

        new obsidian.Setting(containerEl)
            .setName("📣 键控光标跳转「Alt+Shift +I, +K」")
            .setDesc("控制光标在标题、列表、待办、代码块和引用等文本行 或在粗体、高亮、注释、删除等MD语法字符之间 上下跳转。")
        
        new obsidian.Setting(containerEl)
            .setName('📣 键控切换同文件夹内的文件显示「Alt+Shift +U +N」')
            .setDesc('按下快捷键，控制打开同文件夹内的上一文件或下一文件。')
        
        new obsidian.Setting(containerEl)
            .setName('📣 设置标题及粗、斜、删、亮等效果（MarkDown语法）功能')
            .setDesc('启用后，当未选文本时按下Alt + Shift +C加粗 +G高亮 +S删除线 +U上标 +N下标 等快捷键，即开启或关闭 MD语法「格式刷」功能。')

        var div1 = containerEl.createEl('p', {
            cls: 'recent-files-donation',
        });
        var mdText = document.createDocumentFragment();
        mdText.appendText('转换标题语法「待设置」：指定或取消当前行文本为N级标题；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('修改内部链接的显示名称「待设置」：从内部链接里路径中提取名称做为 [[|名称]] 显示。感谢火冷（85399416）增强相关功能；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('调高标题级别「待设置」：将当前标题级别调高一级（最高为一级）；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('调低标题级别「待设置」：将当前标题级别调低一级（最低为六级）；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('调高所有标题级别「待设置」：将所有标题的级别调高一级（最高为一级）。建议：空 QQ:1977878681；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('调低所有标题级别「待设置」：将所有标题的级别调低一级（最低为六级）。建议：空 QQ:1977878681；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('转换标签语法「Alt+Shift+3」：将选文转为或去除 #标签 效果；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('标签双链互转「Ctrl+Alt+Shift+3」：将 [[笔记名]] 与 #笔记名 效果互转；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('转换粗体语法「Alt+C」：将选文转为或去除 **粗体** 效果；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('转换斜体语法「Alt+X」：将选文转为或去除 *斜体* 效果；');
        mdText.appendChild(document.createElement('br'));
        //mdText.appendText('转换行内代码「Alt+D」：将选文转为或去除 `行内代码` 效果；');
        //mdText.appendChild(document.createElement('br'));
        mdText.appendText('转换删除线「Alt+S」：将选文转为或去除 ~~删除线~~ 效果；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('转换下划线「Alt+H」：将选文转为或去除 <u>下划线</u> 效果；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('转换代码块「待设置」：将选文转为或去除 ```代码块``` 效果；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('转换上标语法「待设置」：将选文转为或去除 <sup>上标</sup> 效果；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('转换下标语法「待设置」：将选文转为或去除 <sub>下标</sub> 效果；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('[[]]转为[]()语法「待设置」：将划选文本中的[[内部链接]]语法转为[超](链接)语法；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('[]()转为[[]]语法「待设置」：将划选文本中的[超](链接)语法转为[[内部链接]]语法；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('去除超链接语法「待设置」：将划选文本中的[]()超链接样式恢复为普通文本，即只保留[]内的内容；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('转换无语法文本「Ctrl+Alt+Z」：鼠标点击或划选文本的语法部分，可去除相应的MarkDown语法字符；');
        mdText.appendChild(document.createElement('br'));
        mdText.appendText('获取无语法文本「Ctrl+Alt+C」：去除划选文本中的所有MarkDown语法字符，并写入剪贴板；');
        mdText.appendChild(document.createElement('br'));
        div1.appendChild(mdText);
        
        /*
        new obsidian.Setting(containerEl)
            .setName("📣 是否启用Blut topaz 主题高亮配色支持")
            .setDesc("启用此项后，可以使用Blut topaz主题自带的涂黑、填空等效果。")
            .addToggle(toggle => toggle.setValue(this.plugin.settings.isBT)
            .onChange((value) => {
            this.plugin.settings.isBT = value;
            this.plugin.saveSettings();
        }));*/

        new obsidian.Setting(containerEl)
            .setName('📣 设置彩色文字效果（Html语法）功能')
            .setDesc('点击颜色块调节颜色，在笔记编辑区划选文本后按下「Ctrl+Shift+ 1-5」快捷键，即可转为相应颜色的文本。')

        const textColourPicker1 = containerEl.createEl("input", {
            type: "color",
        });
        textColourPicker1.value = this.plugin.settings.hColor1;
        textColourPicker1.addEventListener("change", async () => {
            this.plugin.settings.hColor1 = textColourPicker1.value;
            await this.plugin.saveSettings();
        });

        const textColourPicker2 = containerEl.createEl("input", {
            type: "color",
        });
        textColourPicker2.value = this.plugin.settings.hColor2;
        textColourPicker2.addEventListener("change", async () => {
            this.plugin.settings.hColor2 = textColourPicker2.value;
            await this.plugin.saveSettings();
        });
        
        const textColourPicker3 = containerEl.createEl("input", {
            type: "color",
        });
        textColourPicker3.value = this.plugin.settings.hColor3;
        textColourPicker3.addEventListener("change", async () => {
            this.plugin.settings.hColor3 = textColourPicker3.value;
            await this.plugin.saveSettings();
        });

        const textColourPicker4 = containerEl.createEl("input", {
            type: "color",
        });
        textColourPicker4.value = this.plugin.settings.hColor4;
        textColourPicker4.addEventListener("change", async () => {
            this.plugin.settings.hColor4 = textColourPicker4.value;
            await this.plugin.saveSettings();
        });

        const textColourPicker5 = containerEl.createEl("input", {
            type: "color",
        });
        textColourPicker5.value = this.plugin.settings.hColor5;
        textColourPicker5.addEventListener("change", async () => {
            this.plugin.settings.hColor5 = textColourPicker5.value;
            await this.plugin.saveSettings();
        });

        /*
        new obsidian.Setting(containerEl)
        .setName('转换文字颜色「Ctrl+Shift+1」')
        .setDesc('设置文字颜色值（#000000）')
        .addText(text => {
            text
                .setValue(this.plugin.settings.hColor1)
                .onChange((value) => {
                this.plugin.settings.hColor1 = value;
                this.plugin.saveSettings();
            });
        });
        */

        new obsidian.Setting(containerEl)
            .setName('📣 设置彩色背景效果（Html语法）功能')
            .setDesc('点击颜色块调节颜色，在笔记编辑区划选文本后按下「Ctrl+Alt+ 1-5」快捷键，即可转为相应背景颜色的文本。')

        const heatmapColourPicker1 = containerEl.createEl("input", {
            type: "color",
        });
        heatmapColourPicker1.value = this.plugin.settings.bColor1;
        heatmapColourPicker1.addEventListener("change", async () => {
            this.plugin.settings.bColor1 = heatmapColourPicker1.value;
            await this.plugin.saveSettings();
        });

        const heatmapColourPicker2 = containerEl.createEl("input", {
            type: "color",
        });
        heatmapColourPicker2.value = this.plugin.settings.bColor2;
        heatmapColourPicker2.addEventListener("change", async () => {
            this.plugin.settings.bColor2 = heatmapColourPicker2.value;
            await this.plugin.saveSettings();
        });
        
        const heatmapColourPicker3 = containerEl.createEl("input", {
            type: "color",
        });
        heatmapColourPicker3.value = this.plugin.settings.bColor3;
        heatmapColourPicker3.addEventListener("change", async () => {
            this.plugin.settings.bColor3 = heatmapColourPicker3.value;
            await this.plugin.saveSettings();
        });

        const heatmapColourPicker4 = containerEl.createEl("input", {
            type: "color",
        });
        heatmapColourPicker4.value = this.plugin.settings.bColor4;
        heatmapColourPicker4.addEventListener("change", async () => {
            this.plugin.settings.bColor4 = heatmapColourPicker4.value;
            await this.plugin.saveSettings();
        });

        const heatmapColourPicker5 = containerEl.createEl("input", {
            type: "color",
        });
        heatmapColourPicker5.value= this.plugin.settings.bColor5;
        heatmapColourPicker5.addEventListener("change", async () => {
            this.plugin.settings.bColor5 = heatmapColourPicker5.value;
            await this.plugin.saveSettings();
        });

        /*
        new obsidian.Setting(containerEl)
            .setName('转换背景颜色「Ctrl+Alt+2」')
            .setDesc('设置背景颜色值（#000000）')
            .addText(text => {
                text
                    .setValue(this.plugin.settings.bColor2)
                    .onChange((value) => {
                    this.plugin.settings.bColor2 = value;
                    this.plugin.saveSettings();
                });
            });
        */
        new obsidian.Setting(containerEl)
            .setName('📣 左侧窗口滚屏幅度')
            .setDesc('按下 Alt+Shift+I +K 快捷键可上下滚动左侧窗口。拖动滑条可调节滚屏幅度（像素值）：')
            .addSlider(slider => slider
                .setLimits(25, 900, 25)
                .setValue(this.plugin.settings.maxScroll)
                .setDynamicTooltip()
                .onChange((value) => {
                this.plugin.settings.maxScroll = value;
                this.plugin.saveSettings();
            }));
        /*
        new obsidian.Setting(containerEl)
            .setName("📣 在状态栏显示 写作进度 → 目标字数")
            .setDesc("启用此项后，在状态栏显示写作进度。拖动滑条可调节目标字数：")
            .addToggle(toggle => toggle.setValue(this.plugin.settings.isShowNum)
                .onChange((value) => {
                this.plugin.settings.isShowNum = value;
                this.plugin.saveSettings();
            }))
            .addSlider(slider => slider
                .setLimits(100, 10000, 100)
                .setValue(this.plugin.settings.maxTry)
                .setDynamicTooltip()
                .onChange((value) => {
                this.plugin.settings.maxTry = value;
                this.plugin.saveSettings();
            }));

        */
        new obsidian.Setting(containerEl)
            .setName('📣 设置字符、标点、状态等转换功能')

        var div3 = containerEl.createEl('p', {
            cls: 'recent-files-donation',
        });
        var charText = document.createDocumentFragment();
        charText.appendText('修复外来文本「待设置」：对 PDF 或 OCR 识别的大段文本进行修复（断行、标点）。感谢 zhl111 建议；');
        charText.appendChild(document.createElement('br'));
        charText.appendText('修复错误标点「待设置」：将笔记中的汉字中间的英文标点修复为中文标点。感谢叶茜彬（7424863）参与；');
        charText.appendChild(document.createElement('br'));
        charText.appendText('修复错误语法「待设置」：修复错误的MD语法，如1。列表、【】（）链接、[[]]()回链等；');
        charText.appendChild(document.createElement('br'));
        charText.appendText('转换路径语法「待设置」：将 c:\\windows 与 [](file:///c:\/windows) 路径语法相互转换；');
        charText.appendChild(document.createElement('br'));
        charText.appendText('简体转为繁体「待设置」：将笔记中的简体汉字转换为繁体汉字；');
        charText.appendChild(document.createElement('br'));
        charText.appendText('繁体转为简体「待设置」：将笔记中的繁体汉字转换为简体汉字；');
        charText.appendChild(document.createElement('br'));
        charText.appendText('列表转为图示「待设置」：选中列表文本，转换为相应层级的MerMaid语法图示，支持修改列表后更新图示；');
        charText.appendChild(document.createElement('br'));
        charText.appendText('转换待办状态「待设置」：转换选文行首的待办状态，顺序为 -[ x-!?><+] 效果；');
        charText.appendChild(document.createElement('br'));
        charText.appendText('转换callout语法「待设置」：转换选文为callout语法样式，即行首补加>[!note]及>符号；');
        charText.appendChild(document.createElement('br'));
        charText.appendText('转换填空「待设置」：将选文转为或去除 {{c1::选文}} 效果；');
        //charText.appendChild(document.createElement('br'));
        //charText.appendText('【选文】「待设置」：在选文两端添加或去除 【】符号；');
        //charText.appendChild(document.createElement('br'));
        //charText.appendText('（选文）「待设置」：在选文两端添加或去除 （）符号；');
        //charText.appendChild(document.createElement('br'));
        //charText.appendText('「选文」「待设置」：在选文两端添加或去除 「」符号；');
        //charText.appendChild(document.createElement('br'));
        //charText.appendText('《选文》「待设置」：在选文两端添加或去除 《》符号；');
        div3.appendChild(charText);

        new obsidian.Setting(containerEl)
            .setName('📣 设置修复断行、选择段句、嵌入网页等功能')

        var div4 = containerEl.createEl('p', {
            cls: 'recent-files-donation',
        });
        var toolText = document.createDocumentFragment();
        toolText.appendText('计算所选结果「F9」：计算所选的四则运算式的结果，并写入剪贴板以备粘贴；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('修复意外断行「待设置」：修复笔记中的意外断行（删除结尾不是句式标点的换行符）；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('搜索当前文本「待设置」：通过搜索面板在当前文档中搜索划选内容；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('选择当前整段「待设置」：选择光标所在的当前整段文本；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('选择当前整句「待设置」：选择光标所在的当前整句（中文）文本；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('选择当前语法「Alt+Shift+K」：选择光标所处的MrakDown语法文本（如加粗、高亮、删除、链接等效果）；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('获取标注文本「待设置」：获取标题、高亮、注释及前缀(#标注\批注\反思)等文本内容；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('获取当前字符数「待设置」：计算页面中可见字符（汉字、字母、数字、标点）和不可见字符（空格等）个数；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('自动设置标题「待设置」：将选文中的单行文本（末尾非标点或数字）转为标题；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('指定当前文件名「待设置」：划选文字后指定为当前笔记的文件名；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('嵌入当前网址页面「待设置」：在行末插入iframe代码来嵌入所选网址页面；');
        toolText.appendChild(document.createElement('br'));
        toolText.appendText('获取相对路径「待设置」：获取当前笔记在库目录内的相对路径；');
        toolText.appendChild(document.createElement('br'));
        div4.appendChild(toolText);


        new obsidian.Setting(containerEl)
            .setName('📣 设置折叠标题、增减空行或空格等功能')

        var div5 = containerEl.createEl('p', {
            cls: 'recent-files-donation',
        });
        var lineText = document.createDocumentFragment();
        lineText.appendText('折叠同级标题「Ctrl+Shift+Alt+D」：判断当前行的标题层级，将正文中同级标题一次性折叠起来；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('折叠某级别标题「待设置」：将正文中某一层级的标题一次性折叠起来。感谢火冷（85399416）增强相关功能；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('删除当前段落「Ctrl+D」：删除当前段落；若在[[]]内会先删除链接内容、在有序列表项内会自动调小后面序号；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('插入有效空行「Ctrl+Shift+Alt+Enter」：插入带有全角空格的空白行，保证渲染时占据一行；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('空格转为空行「待设置」：将两个汉字中间的空格或制表符转为空白行；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('批量插入空行「Ctrl+Shift+L」：在划选的文本行或全文中间批量插入空白行；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('批量去除空行「Ctrl+Alt+L」：批量去除划选文本或全文中的空白行；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('上方插入空行「待设置」：在当前文本行的上行插入空白行；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('下方插入空行「待设置」：在当前文本行的下行插入空白行；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('全文首行缩进「待设置」：在全文的每个行首添加两个全角空格，产生缩进效果；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('当前行缩进「待设置」：在当前行文本的行首添加两个全角空格，产生缩进效果；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('行首添加空格「待设置」：在每行文本的行首添加两个空格；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('去除行首空格「待设置」：批量去除每行文本的行首空格字符；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('末尾追加空格「待设置」：在每行文本的末尾追加两个空格；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('去除末尾空格「待设置」：批量去除每行文本的末尾空格字符；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('添加间隔空格「待设置」：在正文的汉字与字母之间批量添加空格，如 china 中国；');
        lineText.appendChild(document.createElement('br'));
        lineText.appendText('去除所有空格「待设置」：去除正文中所有的全、半角空格。');
        lineText.appendChild(document.createElement('br'));
        div5.appendChild(lineText);

        var div6 = containerEl.createEl('p', {
            cls: 'recent-files-donation',
        });
        var qqText = document.createDocumentFragment();
        qqText.appendChild(document.createElement('br'));
        qqText.appendText('🆗 欢迎大家向蚕子(QQ:312815311) 提出操作需求和建议，我们来共同增强编辑功能！');
        qqText.appendChild(document.createElement('br'));
        qqText.appendText('🆗 感谢Cuman(QQ:35669852)的指导与调试。');
        div6.appendChild(qqText);
    };
};

module.exports = MyPlugin;

/*
var en = {
    loadThisPlugin: 'loading Enhanced Editing plugin',
    thisPluginName: 'ZH Enhanced Editing V',
    helloWorld: '<b>Welcome to the Enhanced Editing!</b>',
    qq: 'View <a href="https://github.com/obsidian-canzi/Enhanced-editing/releases">Github page</a>,Link <a href="http://wpa.qq.com/msgrd?v=3&uin=312815311&site=qq&menu=yes">CanZi (QQ:312815311)</a>',
    close: 'Click here to close the window !',
    FunctionUpdate: 'This Version adds a bilingual interface between Chinese and English, abolition of the "Smart line" function, the conversion of English and Chinese punctuation features optimized for the "Repair error Punctuation" ...',
    setInterTab: '📣 Insert tab [ Tab ]: Inserts the tab effect in the text line',
    whenEnabledTab: 'When enabled, pressing Tab in a line of text inserts four spaces instead of indenting the entire line.',
    convertInternalLink: '📣 Convert the internal link [ Alt + Z ]: Add or remove the [[]] symbol at both ends of the selection',
    SupportBatchConversion: 'Support batch conversion of multi-line text (separated by a newline character) or multiple text (separated by a dot sign).',
    ConvertSynonymousLink: 'Convert synonymous link [ Alt + Q ]: Convert selected text to [[|selectedText]] style before selecting document',
    setSmartPaste: '📣 Smart paste [ Ctrl + Alt + V ]: paste the copied content into MD syntax style',
    isPasteTableURL: 'Paste the table, URL, local path, or code directly into a MD table, hyperlink, or code block format, depending on the type of copied content.',
    setSmartSyntax: '📣 Smart syntax [ Alt + ; ]: Automatically convert, match, or skip various types of parenthesis or code block syntax',
    isYouCanCombine: 'You can combine [( (< ([ "[ \'[ to 〖〈〔『「, or convert common program language names such as dv, qy, mm, CSS, js, ja, ty names (strings) to code block syntax.',
    ConvertCursorMove: '📣 Control the cursor movement with the key button [ Alt+I, +J, +K, +L ]',
    isConvertCursorMove: 'Press Alt+I up, +J up, +K Down, +L right, +U begin, +N end, Shortcut to control the movement of the cursor.',
    ConvertCursorJump: '📣 Control the cursor goto with the key button [ Alt+Shift +J, +L ]',
    isConvertCursorJump: 'Control the cursor to jump back and forth between the text lines such as title, list item, to-do item, code block and reference, or markdown syntax such as bold, highlight, comment, delete and link',
};

var zhCN = {
    loadThisPlugin: '加载增强编辑插件',
    thisPluginName: 'ZH增强编辑 V',
    helloWorld: '<b>欢迎使用增强编辑插件！</b>',
    qq: '查看 <a href="https://github.com/obsidian-canzi/Enhanced-editing/releases">Github 页面</a>，联系<a href="http://wpa.qq.com/msgrd?v=3&uin=312815311&site=qq&menu=yes">蚕子</a>',
    close: '点击此处 可关闭提示窗口......',
    setInterTab: '📣 插入制表符「Tab」 在普通文本行中插入制表符效果',
    whenEnabledTab: '启用此项后，在普通文本行中按下 Tab 键会插入4个空格，不再整行缩进。',
    convertInternalLink: '📣 转换内部链接「Alt+Z」：在选文两端添加或去除 [[ ]] 符号',
    SupportBatchConversion: '支持转换多行文本（需用换行符分隔）或多句文本（需用顿号分隔）。',
    ConvertSynonymousLink: '转换同义链接「Alt+Q」：将选文转换为 [[|选文]] 样式后再选择文档',
    setSmartPaste: '📣 智能粘贴「Ctrl+Alt+V」：将复制的内容粘贴为Md语法样式',
    isPasteTableURL: '依据复制内容的类型，将表格、网址、本地路径或代码直接粘贴为MD表格、超链接或代码块格式。',
    setSmartSyntax: '📣 智能语法「Alt+;」：自动转换、匹配或跳过各种类型的括号或代码块语法',
    isYouCanCombine: '可将[( (< ([ "[ \'[等组合转为〖〈〔『「，或将dv、qy、mm、CSS、js、ja、ty等程序语言名转为代码块、类型词语转为Callout引用语法。',
    ConvertCursorMove: '📣 键控光标移动「Alt+I, +J, +K, +L」',
    isConvertCursorMove: '按下Alt +I向上 +J向左 +K向下 +L向右 +U文首 +N文末 快捷键，控制光标移动位置。',
    ConvertCursorJump: '📣 键控光标跳转「Alt+Shift +I, +K」',
    isConvertCursorJump: '控制光标在标题、列表项、待办事项、代码块和引用等文本行之间来回跳转，或在粗体、突出显示、注释、删除和链接等Markdown语法之间来回跳转',
};

// Code from https://github.com/valentine195/obsidian-admonition/blob/master/src/lang/helpers.ts
const localeMap = {
    en,
    'zh-cn': zhCN,
};
const locale = localeMap[obsidian.moment.locale()];
function t(_str) {
    return (locale && locale[_str]) || en[_str];
};

//t('字段名称')
//以上为 多语言字段名-调用语法
*/