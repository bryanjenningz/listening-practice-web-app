module Subtitles exposing (Subtitle, SubtitleId, subtitles1, subtitles2, subtitles3)

import Random exposing (Generator)


type alias SubtitleId =
    Int


type alias Subtitle =
    { id : SubtitleId
    , videoId : String
    , text : String
    }


generateSubtitleId : Generator Int
generateSubtitleId =
    Random.int Random.minInt Random.maxInt


generateSubtitles : String -> String -> Generator (List Subtitle)
generateSubtitles videoId subtitles =
    subtitles
        |> String.split "\n"
        |> (\lines ->
                Random.list (List.length lines) generateSubtitleId
                    |> Random.map
                        (\subtitleIds ->
                            List.map2
                                (\subtitleId line ->
                                    { id = subtitleId
                                    , videoId = videoId
                                    , text = line
                                    }
                                )
                                subtitleIds
                                lines
                        )
           )


subtitles1 : Generator (List Subtitle)
subtitles1 =
    generateSubtitles "UwRZ8TY2Z4k" """哈喽大家好
欢迎来到聊聊东西
我们聊东西文化
也聊很多东西
我是 Candice
今天又是我一个人
因为现在是我们的国庆假期
一个星期的假期
所以大家都休息啊
出去玩什么的
我也不想去找 yifei
或者是别的朋友来跟我一起
录音我想他们可以多休息一下
那今天就是我一个人跟大家聊一聊
其实我也有好几个话题备选
但是刚刚打开手机
打开微博
看到一个热搜话题
就是七个省将出现三十七度以上的高温
包括我在的城市也是特别热
这几天三十七度
明天三十八度
真的太热了
现在已经是十月份了
就是让人难以置信的为什么
连续的高温
而且整个夏天也是非常的热
包括我有听说欧洲
虽然我没有去过欧洲
欧洲整个夏天非常热
然后欧洲人办公室有空调
家里没有空调
那晚上睡觉也是非常热
我听 yifei 说
因为 yifei 以前不是在英国留学吗
夏天的时候就是很凉快吗
夏天可能二十多度
哇我觉得这真是太舒服的夏天了
这个温度就是刚刚好啊
你也不会出汗
然后非常舒服的夏天
但是现在也不复存在了
我还看到以前
两个月可能看到说美国加州也是
缺水干旱
是特别严重的那种缺水干旱
包括中国在内农作物减产
然后因为太热没有没有水吗
缺水那些蔬菜啊
水果啊一些树木啊
他们都长不好
那当然物价就会上涨
物价上涨当然有很多的原因了
包括疫情的原因
有很多别的原因
但是我觉得今年夏天
这个天气也是原因之一啊
所以为什么我们的夏天越来越热
持续高温
没有雨持续干旱
真的让人觉得怎么回事啊
这个地球怎么了
因为这样
我就想自己去找一些纪录片看一下
今年初我开始看一些纪录片
印象比较深刻的两个纪录片
一个叫 chasing ice
中文翻译叫逐冰之旅
那一个摄影团队在
南极拍摄这个冰的变化
天气的变化
那他们拍摄的结果就是发现
变化真的是出乎意料的惊人
最后的数据统计就是过去十年
这个整个冰融化的总量
比过去一百年都多
所以可能很多的南极的一些动物
一些生物他们都失去了原来的家
北极熊啊
企鹅啊因为这个气候变暖
他们的食物减少了
他们捕捞食物的过程变得更加艰难
他们更难生存了
然后冰融化之后
当然整个海平面要上升
然后冰融化之后
可能也会导致一些病毒
古老的病毒再次出现在世界上
可能温度高
也会更适宜一些病毒的繁衍
生殖
那我看完这个逐冰之旅之后
又看了一个叫 chasing coral
他们好像是一个系列的纪录片
这个中文名叫追逐珊瑚
看完之后也让我觉得挺绝望的
比这个 chasing ice 更让人觉得绝望
因为这个摄影团队
他们知道珊瑚在每一年减少
但是他们去拍摄的过程发现
珊瑚减少的速度
死亡的速度
比他们想象的快的多的时候
真的让人感觉到非常绝望
因为珊瑚在整个海洋系统里面
是一个非常基本保持海洋
生态的一个生物
那如果海洋里没有了珊瑚
那些小动物他就没有了家
没有了食物的来源
那他们就会减少数量
相应的大的动物没有了
这些小动物去当食物的话
他们也会逐渐的灭绝
所以生物多样性受到影响
物种也会因为这件事情有灭绝的可能
所以我不知道每次看完纪录片
就会觉得人类很渺小
人类出现的这短短的这段时间里
对整个世界
对整个地球产生的影响太大了
改变太多了
我很难说这样的改变
是好的还是不好的
你看这个追逐珊瑚也是会发现
海洋里面的垃圾太多了
塑料瓶塑料袋
各种塑料出现在纪录片的镜头里
他们没有办法短期内得到分解
一直在海洋里堆积产生这些垃圾
而人们又
没有一些好的办法去解决这个问题
挺难的
当年发明塑料的人
大家都觉得是一个很聪明很有用
很轻很耐用很便宜的一种材料
但是没有想到这么多年过去之后
他会成为一个问题
但是现在可能
人们还没有一个很
好的方式去解决这个问题
再说垃圾分类这个问题
我看的另一个纪录片
我不太记得他的名字叫什么
他就是在讲这些垃圾是怎么处理的
很多国家比如说包括日本啊
比如说一个塑料瓶
他的瓶盖和他的瓶身
好像都应该是分
在不同的垃圾种类里面
分的很细
我看的这个纪录片是讲的美国的情况
我不知道日本或者是其他的国家
是什么样的情况
这个专门做垃圾回收的
做的很大的一个公司说
这些垃圾分类了之后也没有办法
这些塑料瓶
塑料袋
就算是已经分类到塑料这一类了
但是他们能做的只是把这些垃圾
放到一起
可以把它填埋在地上
就是埋在地下
让他几百年几千年慢慢的分解
或者是运到一些发展中国家填埋
以前中国其实是很
很多一些发达国家呃的垃圾的目的地
这很多的呃
外国的垃圾会运到中国来填埋
但是现在可能因为中国的垃圾太多了
中国政府
有一些措施会阻止这样的情况发生
所以到最后
就算你很
用心的做好了垃圾分类这件事情
整个地球还是被这些垃圾包围了
人们没有想出更好的办法
所以我们能做什么呢
这个是我们应该想的问题
可能听到这有的人会想哈
你操这么多心做什么呀
我们都是普通的人
我们能做什么呢
现在全球变暖了我可以做什
我们什么也做不了
我觉得大部分人都是这样想的
包括我自己以前也是这样想的
我不是以前还讨论过吃素这个话题吗
那有一些我认识的人
他们吃素是因为他们的宗教信仰
或者是因为他们觉得动物很可爱
不应该杀死动物们
但还有一个很重要的原因
就是他们觉得他们是在保护环境
因为饲养的家畜
其实排放了很多的二氧化碳
一氧化二氮这样的东西
排放了很多的废气会导致全球变暖
其实他们就是在用自己的力量
为啊这个全球变暖做一点点贡献吧
保护环境
减少全球变暖
做一点点他们可以做的事情
但是并没有说要求每个人去吃素
因为我自己也做不到
但是如果你知道了这些事情之后
可能你会
想办法去做一点点什么来保护地球
保护环境
其实有时候如果心情
不是那么好的时候
我会看一些这样的纪录片
看一些比如说地球的繁衍过程
然后一些动物世界
植物世界这样的一些纪录片
就会让我烦恼减少一点点
因为我觉得这个世界太大
这个宇宙太大了
人的每一天的烦恼那么那么的渺小
你在烦恼你每天吃的东西
你的工作
你的学习
放在整个地球看
放在整个宇宙看
这都是特别特别小的事情
人真的很渺小
能做的事情很少
但是如果每个人都这样想的话
都什么都不去做的话
整个环境会越来越差
可能
我觉得对我们这一代的影响还不太大
到我们的孙子孙女那一代人的时候
会不会
他们已经都没有干净的空气可以使用
没有干净的空气
干净的水
不可以自由自在的在那个草地上奔跑
我不知道会发生什么
如果我们从现在开始
做一点点我们力所能及的事情
让身边的人知道这些事情
会不会间接的影响一些有能力的人呢
比如说我可能没有能力
但是我在我的播客里说了
那在我的听众里有一些很聪明
很厉害的人
他发明了一个
可以分解塑料的一种东西
那是不是就是一个很大很大的成就
很大很大的帮助呢
我看了这些纪录片
或者是别人听了我的播客
他决定
可能从明天开始他不用塑料袋
他每次去超市买东西
他都用那个环保袋
用布的袋子
或者是什么别的可以更容易降解
或者循环使用的材料
我在韩国的时候
那都很多年以前了
韩国人就已经开始
带自己的杯子去买咖啡
带自己的杯子买咖啡可以少
那个时候可以少五百韩币
就是带自己的杯子去买咖啡
少不了多少钱
但是又很麻烦
因为你要带个很大的杯子去
然后背着他回家还得洗对不对
很麻烦但是那个时候已经有
很多韩国人这样做了
后来我的一个韩国学生还告诉我
他们还会用自己的吸管
可能是买一个金属的吸管
每一天洗
这样就不用那些塑料的吸管
我觉得这些都是很好的一些习惯
当然可能我自己平时不喝咖啡
所以我很少会去咖啡店买咖啡
但是我很喜欢喝那个三得利的乌龙茶
污染挺严重的
你想每一瓶
乌龙茶都用一个塑料瓶装着
那如果我非得喝的话
我可以比如说买个大瓶子的
那就是可能一个大瓶
差不多是三个小瓶
那我买个大瓶
可能好一点点吧
就是减少一点点塑料瓶
这个可能是我能做到的一些事情
或者是我减少使用空调
就早上不要用
中午最热的时候用一用
然后晚上睡觉的时候
太热的时候用一用
还有就是多用走路啊
或者是骑共享单车啊
用公共交通更多
其实这个是因为我不喜欢开车
但也要看国情对吗
如果你是在欧洲
或者是在美国
你们可能必须使用到车
因为有时候你坐地铁
或者是坐公交车不太方便吧
你必须开车
比如说你可以一个星期去一次
多买一点这样就是减少开车的次数
我觉得我还要再去想一想
我还可以
在我的能力范围内
做一点什么小小的事情
可能我是一个无知的人
我不是一个啊很有能力
可以用我的科学技术去改变
现在这个情况
我也做不到
那我只能看看我可以做点什么
比如说减少用塑料的东西
还有一些人
的观念是觉得全球变暖是一个骗局
是一些政客为了自己的目的
编造的谎言
我不知道全球变暖是不是真的
这些数据是不是真的我不知道
我只是根据我自己的经验来讲
可能我小的时候的夏天没有这么热
我小的时候的冬天下雪会更厚一些
下雪的时间更长一些
现在的冬天下雪很快就结束了
可能下的雪第二天第三天就化了
非常非常少
作为一个想让地球更好的人
或者是想让我们的后代
生活的更好的人
我们可以做一点点力所能及的事情
那么这么一点点力所能及的事情
可能都会对保护环境起到点点的帮助
那这些东西堆积成一座小山
可能会有一些影响
最后谢谢大家的收听
如果是我的中国听众
当然我祝你十一假期快乐
好好的休息
因为工作很累了
休息好了可以更努力的去工作赚钱
那如果是我外国的
听众的话希望你们有一个好的周末
那我们下次见啦拜拜"""


subtitles2 : Generator (List Subtitle)
subtitles2 =
    generateSubtitles "jKPlNHHYKZo" """大家好欢迎来到聊聊东西
我们聊东西文化也聊很多东西
我是 Candice 大家好
我是 yifei
哈哈又很久了大概两个月了
又找到 yifei 跟我一起录
哈哈哈
因为这次这个话题
我觉得找他聊挺合适
为什么我想聊这个
是因为最近我看那个一个网友
他发的一个照片
现在不是医院都有电子屏吗
然后就感觉
现在新生儿的名字跟我们以前完全
不一样就是很文艺的那种名字
而且都是个字或者个字
很少有两个字对不对啊
然后我就看了一下我们公安部
去年的全国姓名报告
然后也找了一下
美国的和澳洲的政府网站
中国的名字和英文名字
这个几十年前或者到现在
这个流行趋势的改变
中国人跟英国人
美国人起名字有什么不一样
我们今天聊一聊这个话题
还有一个话题就是
关于中国人起英文名这个话题
好像比如说我们的邻居日本
韩国印度
他们都很少会给自己起个英文名嗯
可能你说日语的发音稍微简单一点
那可能念起来比较容易
但是印度的名字
我觉得很多印度朋友们的名字很长
很难念嗯
而且韩语的名字还有收音
对
很多人我觉得不知道怎么发那个收音
但是别人就是坚持用自己的本名
为什么中国人
这么喜欢给自己一个英文名
可能从学校开始老师就给英文名
你还记得你第一个英文名是什么
是你老师给你的吗
还是你自己老师给我的
这哈哈哈
给什么名字
不好意思说哈
哈哈
他给我起的名字叫珍珠
叫 pearl
对我当时觉得可美了
我说天呐
这个名字是我们班上最美的名字
因为珍珠不是很漂亮吗
又很珍贵
哦
然后我就觉得我的名字可美了
那你到什么时候才觉得这个名字有点
你不喜欢要换一个
我我好像是觉得他不好念
还有什么原因我忘了
后边就一直没有用
因为也没有什么机会用后面
就没有用它了
这名字真的很难念
中国人念 r 和 l 都有点问题那个
名字
对然后班上别的同学比如说 pearl
他全部会念 par
par 这样哈哈哈
就不好听你知道吗
哈哈哈
所以我就没有用了
嗯那你什么时候改了
第二个或者第三个英文名
你改了很多次吗还是
哦我跟你讲我的第二个名字能震撼你
呵我大学的时候
然后就跟人家讲说我的英文名
难以启齿
什么呀
就是好好奇哦
我当时沉迷于哈利波特
哦
然后你能猜到我叫什么吗
我知道你这个名字啊
你曾经在韩国也用过这个名字啊
你不记得了对
叫 Hermione
你对呀哎
这有什么不好意思说的呀
不我我给你举个例子就好比
嗯
我跟你说我的名字叫
有没有一个什么戏剧里面
好比如说我跟你说亲爱的
我的名字叫林黛玉
啊林黛玉都还好了
林黛玉都还好对啊
比如说我叫什么铁扇公主
哪有这样子的感觉
没有这个感觉啊就是哈利波特
反正就是他不常用你知道吗
他就是一个影视剧里边的一个名字
而且他是一个很很古老的一个名字
哦我知道了别说跟你说我叫妈祖
搞笑
你觉得这是合理的吗
哈哈哈
这风格完全不一样好吧
妈祖
咋不一样啊
一样哈哈哈
这种很奇怪所以我都能想到
我以前跟那些嗯
我们学校那些留学生介绍说
my name is hermione
你能想象人家就那五雷轰顶的感觉
笑死
就你有病吧
哈哈
我还有比你更搞笑的名字
所以你这还好哈哈
叫什么
等会跟你说 然后后来呢
后来在韩国
你你记得那个时候我不是
先认识的那个 francisca 吗
嗯
然后我就跟他说我说我这个名字就是
太长他不好念
而且他太
太古老了
我就说你有没有一个什么好的想法
可以叫什么
嗯然后
他当时想了一个他说叫 teresa
我说 teresa
很长哎这个名字我是两个音节就好了
嗯
然后他就
我们俩在那商量他就随便想他说
嗯要不 mandy
就是 mandy 哎我说可以啊这个很好念啊
然后就就接受了就这么我就接受了
然后就叫 mandy
你也太容易接受就是这么容易就对啊
就是
英文名反正又不是我自己的真名喽
就怎么好叫怎么来了
对就是我觉得
起英文名好像很少
会考虑这个背后的意义
对不对就大概是听着好听
对
所以我第一个英文名也是我初中的
英语老师给我的
我当时就不喜欢
哈哈哈哈
所以我第一个英文名叫 susan
哈哈哈
susan 还好哎
就
不知道
就给我的感觉是年纪比较大一点
但是我没有冒犯的意思
就可能如果你叫 susan
我没有意思说你老
但是我的印象里好像就是
我觉得我只十二岁
为什么要给我一个好像三十多岁
或者以上的人这样的名字
嗯嗯
但是确实
我的印象也是应该 susan
是一个比较成熟一点的女性的名字
对对对
或者是很多年以前比较流行的小孩名
感觉不是那个时代流行的名字
所以我自己就非常不喜欢
我就作为一个十二十三岁的小孩
你知道我给我自己改了一个什么名吗
自己在那个字典上到处翻
因为中国人起名字喜欢有意义吗
所以我就一定要找一个
有什么特别意思的然后找到一个词叫
bluebell 哈哈哈
你知道什么意思吗
啥东西啥哈哈哈
blue 就蓝蓝色的那个 bell 就是那个门铃吗
那个铃
哈哈哈哈对不起我笑的好大声哈哈哈
这是一种
花吧好像是叫蓝铃花
蓝铃花我知道
然后当时觉得哇这么特别的花
又是蓝色的这个名字又没有人叫
我就要叫这个
然后这个名字我还叫了好几年
哈哈哈哈哈哈
绝了对对对
然后后面才改成 candice
因为我觉得是一个比较普通一点
然后也比较好听一点的名字
是后来才改的
每个人都有黑历史我跟你说
是是是
那
你有没有听过可能比较奇怪的英文名
你觉得就是你去英国的时候
有没有一个英国人
他的名字让你觉得有点特别
英国人的名字
嗯
哎我但他不是英国人他是法国人
然后那个名字我都有点不会念
是 x 开头的
哦
x 开头
那个名字
嗯嗯
我都不会念我怕
念不好就我当时一听就蒙了
我说这名咋念啊
哈哈哈
我一直不敢叫他你知道吗
对
我想说在法语应该有个特别的发音吧
但英语我真的不知道怎么
那你知道我的名字也是 x 开头吗
就是我的姓是 x 开头
所以很多人
很多外国人不知道怎么念我的名字
不会念对
就觉得很奇怪
所以这也是为什么
很多中国人有一个英文名
对因为有些音他真的发不出来
嗯嗯嗯
对你有没有记得一些中国人
或者一些亚洲人
用一些比较奇怪的英文
有
太多了
但是我说会不会不太好
我觉得我也觉得
是不是好像说这个不太好
哈哈哈
我以前有一个是可以说的
我以前就是
你知道在那个培训机构
做英语老师的时候吗
然后就有一个学生
一个女生很漂亮的一个年轻的妈妈
嗯然后我们自我介绍的时候他就说
他说 my name is sugar
然后我就
哈哈哈
我当时当下哈哈哈没有说什么
后面我就问他
我说你学英语你是不是
打算以后要出国呀
他说嗯因为他可能要带儿子
就是陪读嘛
儿子去国外念书去陪读
我说好那如果是这样的话
我就我私下里跟他讲的
我就说这个英文名啊
咱们出国最好就不用换一个嗯嗯嗯
然后
他就说啊怎么了吗
这个名字不好吗
我说
就咱们中国人听起来是挺甜美的
叫 Sugar 我说但是呢
他可能你去了国外之后
他也许会有一些不太好的别的意思
是的我就说
最好是咱们就换一个稍微
就是常用一点的英文名字
他
然后然后他就说行
他说那我再回去找一下
他找找找找找然后回来就改了
他可能自己上网查了
嗯
改了一个正常的
嗯叫 iris
iris
那那还蛮正常
对对对嗯嗯蛮好听的一个名字
嗯嗯嗯
怎么说呢就是如果你去国外的话
可能还是要先在网上查一下
这个名字会不会有什么不好的意思
或者和一些不好的词的音很近
嗯稍微避免一下啊
对啊对啊嗯
那我有其实问了我的美国的学生
澳大利亚的学生
啊不同国家的学生
然后他们都
觉得说英文名起名字没有想那么多
大概就是好听
应该是啊
嗯然后我看了一下这个美国政府网站
二零二一年最流行的女孩的名字
你知道叫什么吗
我猜一下啊
因为我以前有看过哦
就我
我生宝宝之前我有翻过
哦
因为那时候不知道男孩女孩吗
所以我都看女孩的名字是不是 olivia
对 哈哈哈
不是吗
对有点出乎意料我觉得这个名字嗯
我没有认识一个人叫这个名字
你有认识吗
我也没有
可能我们认识的外国朋友太少了
哈哈只能这样讲
没错没错我们孤陋寡闻你知道
对所以是二零一九年到二零二一年
三年
都是这个名字排名第一
啊这么厉害
对那二零一四年到二零一八年
五年另一个名字排名第一
你再猜一个
女生
Emma 对
哈哈哈哈哈哈
我也蛮喜欢 Emma 这个名字
我也喜欢
就是可爱的漂亮的小女孩那种感觉
嗯对
二零一一年到二零一三年也是一个名字
连续三年排第一
天呐这这个太难了我想想
嗯
Sophia
嘿嘿你为什么都猜对了
你是看了吗
你是看了吗
不是因为 Olivia 我是有点印象
我在我在
那个刷抖音有一次刷到有人说
Emma 是我知道啊
但是 sophia 真的是我瞎说的
哈哈你太厉害了
下一个我觉得真的好
我的妈呀
我看是多少连续多少年排名第一
一九九六年到二零零七年连续十一年排名第一
一个女生的名字
elizabeth 吗
没有这么复杂短一点 charlotte
还要短一点
amelia emily
对 嘿嘿嘿
哈哈太恐怖了十几年啊我的妈呀
可是 Emily 就是个好听的名字来的嗯
是嗯那我们来看男生
男生是二零一七年到二零二一年都是一个名字
第一名
Liam
对
我
其实你说呀我怀宝宝的时候研究过
哦
其实我有一个新西兰的学生
他也是个十五岁的小男孩
他也叫 liam 嗯
但我第一次听这个名字
我以为这是一个很少用的名字
没想到是一个
对我身边没有接触过谁叫这个名字
对啊我就觉得啊怎么会是这个名字
可能如果我们生活在
国外的话就会常见一些对
我们再往后倒一点倒一个
哇哦这个名字
看一下啊
哇我惊呆了
哈哈哈
什么
这个名字从一九五四年
到一九九八年只有一年掉下来第一名
五四年到九八年连续霸榜这么多年
daniel
不
michael
对惊呆了
为什么可是 michael 是好听的呀
是好听但是
嗯不至于流行这么多年吗
这个潮流不是老在变吗
女生的名字变了好多次
这个男生名字
五四年到九八年
多少年
可能哎
我我觉得因为国外不是有很多
呃儿子会继承爸爸或者爷爷的名字吗
比如说儿子就叫什么什么什么 junior
啊
什么什么 junior
the third 这样嗯嗯嗯
所以他们就会继承老一辈的名字
可会不会有这个原因
有可能
那为什么女生不这样
不知道
可能妈妈们就会觉得不行
我的小公主就要特别的名字
然后爸爸们就觉得没有关系
我的名字就很好听
你就用我的
哈哈哈哈嗯嗯
好其实我看了一下澳大利亚的情况
也差不多
你给你自己孩子有起英文名吗
有啊
叫啥呀
嗯
哈哈哈
叫 Ethan
也是一个流行算流行的名字哦
就是为什么呢
是因为
我觉得男孩子就是希望他健康强壮吗
所以你知道中国人起名字
他会去看他的含义
然后我就翻了一下就翻那个名字啊
就翻到就是 ethan
是表示就是比较强壮的那种意思
哦
然后然后我我
我其实没有刻意要验
证是不是有这个意思的
嗯然后我那天在学校
我们学校不是有很多外籍老师吗
嗯然后那天我们在聊天
他然后那个老师就问我
他说他是个英国人
他问我他说你孩子叫什么名字
我说他叫 Ethan
然后他说哦 strong
哦
然后我说哎
好像还真的是有这个意思哈嗯
然后我说对啊对啊就是要 strong 的意思
嗯我说也不需要 smart
就是 strong 就好了哈哈
哈哈哈哈哈哈哈
你想把别人笑死是吗
我是
我是一个很糟糕的妈妈
对
OK 好那我们看就是说英文名
那我们说回到中文名了
那你给你儿子起中文名什么思路呢
呃就是我希望
他不是一个太普通的名字
但是绝对
不可以用生僻字嗯
我是肯定不会给孩子
用生僻字起名字的
为什么
我觉得没必要
因为你的名字是人家要称呼你啊
你搞得大家都不知道怎么念你的名字
干嘛呢你可以这样做
但是在我这我觉得没有必要
而且你知道他本来就是复姓吗
他的姓就是两个字
那是假复姓
就已经
哈哈对了对了
但是怎么办
他从爸爸那一辈开始都是这样叫的
然后所以说当时就讲的说
名字特别是要写起来简单一点
不要笔画很多
嗯嗯然后我就这个思路
然后那天我是
因为你知道
我以前一直以为他是个女儿
你知道吗
所以我想了很多女儿的名字
你为什么有这样的错误的想法
不知道我做梦也梦到是个女儿
然后然后
但是儿子的名字是
我当时就是在网上翻呢
突然我就觉得嗯
这个名字特别好意思也特别好
我就选定了他嗯哼
然后选了一个备选
所以儿子的名字是我一眼就看中了
我就说如果是儿子就叫这个
女儿的名字
写了很久很久都没有定下来
那你给儿子起名字的时候
有没有算生辰八字什么的
就吉不吉利啊
什么五行缺水啊缺土啊这种
完全没有
就是就是《论语》里面的一句话
然后就定了
你觉得现在就是你儿子
幼儿园或者你身边的那些小朋友
他们的名字叫什么奇怪的字吗
或者是都喜欢用什么字
子
子
儿子的子
然后木字旁一个辛苦的心
那个梓
紫色的子嗯
然后还有芷
草字头一个停止
芷就周芷若那个芷
芷
对
涵和轩
嗯
好那我们就回到中国
公安部的二零二一年全国姓名报告发布
新生儿姓名
二零二一年男生最多人用的一个名字叫沐宸
沐是那个沐浴的沐
嗯宸是星辰的辰
上面还有一个宝盖头
天呐
这就是我同事儿子的名字一模一样
两个字
好第二名是浩宇就是浩瀚的宇宙
浩宇哦嗯嗯
第三名还是沐辰
只不过第二个辰字没有宝盖头
就是星辰的辰
嗯嗯
哇我看了这些名字全部都好文艺哦
就感觉是那种青春疼
痛小说里面的名字
哈哈哈
哈哈哈哦
然后女生女生去年第一名是
若汐
哇我感觉我回到宫中了
若汐
若就是好像
啊
啊
汐就是三点水夕阳的汐
若汐
哦那个汐
嗯为啥呢
对但是为什么用这个汐
我觉得可能是用的这个字的人少吧
是不是因为这个汐就是
潮水吗
对啊就是潮水的意思啊
没有什么很特别的意思啊好听吧
若汐好像潮水一样
哈哈对啊
来势凶猛
然后
哦然后第二个名字是一诺
我怎么记得刘烨的儿子叫一诺呢
是不是啊
女孩吗你是说女孩叫一诺女孩
你知道为什么吗
为什么呀
因为一诺千金
千金不就是女儿吗
哦
也有道理哈
第三名就是很普通
我觉得能想到的女孩的名字叫艺涵
艺术的艺
涵养的涵
艺涵
嗯嗯啊
还蛮好听的
但就跟我们那个年代就挺不一样
全是三个字或者甚至四个字
我那个时候
我觉得我同学还有挺多是两个字的
你有吗
对有啊肯定啊
嗯现在完全没有啊
为啥你觉得
对
想要特别嘛
可能想赋予孩子名字更多的意思
就是其实我觉得中国人起名字
不是所有
但是很大一部分
是把对孩子的一种
期望和祝福包含在名字里面了
所以他多一个字
肯定就多一层祝福和期望吧
我推测啊
解释的好好哈哈
多一层祝福
此处应该有掌声
可能我这种没有孩子的人不能体会
哈哈哈
那我们再来看一下嗯
你觉得全国使用最多的单字就是
不管男女
在名字里有的一个字
这个字最多
华
没有华
前十都没有华
嗯
伟
对
伟哈哈哈哈
一共有三百多万人有伟这个字
太多了
第二名是一个比较偏女性的字
英
你再猜
秀
嗯
敏
敏感的敏
敏
哦嗯
对有两百七十多万对
我怎么没想到这个字
这个字很多人用哎
嗯对
所以这两个字
是全国最多人用的两个字
但我觉得
可能在新生儿里面用的还是比较少
可能就是以前我们上辈的人
对对
那你有没有遇到过就是奇怪的
外国人的名字
就是外国人会给自己取一个中文名吗
就像我们中国人给自己取一个英文
名一样
嗯嗯
现在网络上有一些
中文说的特别好的外国人
网红吗像是马思瑞呀
阿福啊
阿福是德国人
马思瑞是美国人
然后还看过谁啊叫杰里德吧
好像是一个加拿大人
他们都有一些自己的中文名
然后都算是比较正常的中文
名
在你们那个大学的时候
交换生他们有中文名吗
有
那时候大家都给他们瞎起你知道吗
其实
最方便的就是把他的英文名字直接用
音译过来
就是根据他的发音
这个是最简单的
嗯然后嗯
我们那时候也比较善良你知道吧
也不会给他起个很奇怪的名字
嗯但是他们有时候就会要求说
我希望我的名字听起来好像
将军一样的那种
你知道吧
然后我们说好那你就叫赵子龙
哈哈哈哈不知道这是什么意思
你跟他说了他也不知道
哈哈
不亲爱的他会查的
他说你给我写下来
然后他就去 Google 然后查
他说天呐这是一个这么厉害的将军
好我就叫赵子龙
哈哈
他就很喜欢
那怎么办呢他已经这么喜欢了
对对对
但我觉得也没有什么问题啊
叫这个名字没什么问题
对啊现在也有人叫什么什么子龙嘛
这个很正常
因为我教中文吗
就我会常常遇到这种情况
我要不要给我的外国学生
起一个中文名
因为毕竟中国人有时候英语也不太好
嗯他也叫不出别人的名字来
或者不是英语是一个更难的语言
就更不知道怎么发音了
嗯
但是我的想法是就像你刚刚说的吗
这个名字是父母给孩子的祝福
嗯嗯那既然是祝福
为什么我要改一个名字
嗯嗯我为什么不用我的本名呢
那为什么印度人或者是日本人
韩国人大部分他们都
在国外用自己的本名呢
那么长的名字也很难念
但是总有办法的
嗯
比如说啊
我有一个澳大利亚的朋友
他是一个华裔吗
他的名字是三个字嗯
那第一个字很难发音
可能第三个字很难发音
对于外国人来说
中间一个字好发音
那大家都叫他中间那个字
都叫他 chin
chin 这个字还比较好发音
取他名字中的一个字
他没有英文名
嗯还有比如说我吧
我的名字可能我的姓
我的姓是 x 开头
可能很多人觉得很难发不知道怎么发
但我的最后一个字 bei
就很好发音
嗯
嗯我觉得没有人不可以发 bei 这个字
所以外国朋友可以叫我 bei
但是因为我用 candice 这个名字太多年了
我的外国朋友都已经这样叫我了
我我现在让他改一个名字
他们也会觉得很奇怪
所以我就觉得啊
如果以后我有孩子
我就不会给他起一个英文名
就是中文名
这样不是很酷吗
同时也是一种文化自信啊
嗯对
名字虽然难
但是你也试着念念呗
英语也很难呢
我们也试着在学
全世界的人都在学英语
我们也学挺费劲的
对吗
所以我觉得
如果一个英国人
或者一个美国人来中国
他的中国朋友念不好他的名字
我觉得他们不会怪他
也会觉得有点可爱吧
所以是一样的
所以我的想法是
等到我的学生学中文到一定的程度
如果他自己愿意
他可以根据
他自己的情况来起一个中文名
因为这个时候他已经对汉字有了解
对汉字的含义可能他自己可以去找了
但刚开始的时候他什么都不了解
我给他一个名字
也许是他不喜欢的或者不想要的
对吗
这个是我的想法对
嗯对
我是觉得有一种新鲜感你知道吧
因为你
自己中文的名字是爸爸妈妈起的
你自己是没得选的嗯
但是你在起英文名的时候
你可能可以根据自己的喜好啊
或者什么来说
你给自己起一个
那时候完全就是因为新鲜感
好奇觉得好玩来的
嗯
这样来的
所以说才会起什么 pearl
hermione 这种名字你知道就是我为了好玩
也没有真正拿来使用
像我后面真正要开始使用了之后
你会发现别人念你的名字念不出来
嗯或者念的很奇怪
就是你想说哎呀算了吧
你不可能让
别人都来学怎么念你的名字
没必要了他也念不好
所以就起一个
普通一点正常一点的英文名就好了
其实这也算是一种体贴的方式吧
我觉得就像
你刚刚说不要给小孩子起一些生僻字
因为别人也不不知道怎么读呀
就是很为难别人对
哦对对
所以有一个英文名
可能有时候就是为了大家方便叫你
嗯对的是是
好那我们再聊一下别的国家的情况
因为我有很多越南的学生
我妈很喜欢看排球吗
就是有一天
我看那个中国队和越南队的排球比赛
然后我就听到他们
说那个越南人的名字的时候
就说一个人叫啊
陈氏清水吧
好像叫陈氏清水是四个字
然后我就问了我越南的学生
他说现在很流行就是女生会有四个字
但是这个氏
这个字很特别
这个氏就好像我们姓氏的氏
那个氏
嗯嗯
其实姓氏的氏的意思就是姓的意思对吗
比如说王氏
就是姓王的人
李氏就是姓李的人对吗嗯
对那我们其实在以前
古代的时候也有这种说法
比如说王李氏那就是你的老公
你的丈夫姓王
你的爸爸姓李
所以你可以叫王李氏这样
但现在也不用了
但是他说在越南就是很多女生会叫氏
就是姓的后面加氏再加名字
所以会有四个字这样
好特别啊哈嗯
我就查了一下他们排球队的队员名单
发现每一个人都有氏
每一个
哇
惊呆了我
就还有这种流行
啊
对对对然后他就说
嗯
这种是可能比较传统一点的
起名字的方法
可现在很多人也不喜欢这样起名字
但是但是还是使用的很广泛
我就觉得哇
好有意思
好特别啊
我们都没有这样用哎
古代的时候这样用
嗯对对对我们现在很少这样用
所以我也想
可能别的越南的朋友也可以告诉我
现在你身边认识的人
你们也用氏这个字吗
你们起名字是怎么起的
哦亲爱的嗯
我突然想起来
你刚刚问我说外国人有意思的名字
我想起来
嗯
以前我知道的有个我不认识他
但是我知道他这个人
他的英文名叫 joshua 嗯
然后他的中文名叫乔喜娃
哈哈哈好可爱哦对
我觉得他好聪明啊
然后他整个人说话也是那种很幽默
很搞笑的风格
嗯
那他的名字是他自己取的
还是他老师给他的呢
这个我就不知道
因为我只是就是我们在一个微信群里
我看见他说过话
然后他叫乔喜娃
我觉得好搞笑啊而且一下就记住他了
可爱哦可爱对
哈哈哈哦
这这是个很好的例子哈哈哈
嗯
那我们最后聊的这个话题
其实是在我以前的播客里面已经
聊过的就是
在日本或者在一些西方国家
女生结婚之后不是会跟他的老公姓吗
就是会改他的姓改成老公的姓
但是在中国我们没有这样的事
就是你姓什么就是姓什么对吧
对
建国以后新中国成立以后就没有了吧
据我了解的话中国是冠夫姓
刚刚我提到越南那个女生的
姓名那个氏
我刚刚提到是王李氏吧
我的例子王李氏
那是说你爸姓李
所以你一直叫李某某
但是你的丈夫姓王
所以之后别人会叫你王李氏
但是我们一直都没有那种要改姓
比如说你以前叫李某某
然后你改成王某某
但这是据我了解
可能我孤陋寡闻
也不要误导大家哈哈
但新中国成立以来的话
我们是一定没有这样的情况
就是你姓什么就是姓什么
结婚之后不会改姓
嗯我觉得最早最早
为什么要灌肤信呢
可能是最早最早最早那个年代
女孩子是没有什么劳动力的
就是可能他不是养家的那个人
所以家庭呢以男性为主
所以就让女生冠了夫姓
就像我们中国以前说的
你嫁到别人家去
就是你成了别人家的人
所以你冠人家夫姓
后来呢就是咱们国家
应该是也是倡导男女平等
就是女生和男生
同
具备同样的劳动力和工作机会的时候
女生也能养家
男生也能养家
我们提倡男女平等
所以就把这个作为一种
封建的习俗一并去掉了
然后国外西方国家呢
可能西方国家觉得说
我并没有把他当成一个歧视女生
或者说男女不平等的这样的一个事情
他就就是一个风俗就是一个习惯而已
嗯没有给他加上一种就是呃
歧视女性的这种成分在上面
没有这种意思
所以
他们也就没有刻意去在意这个东西吧
我猜测嗯
而且是
但其实我觉得没什么
是而且现在比较流行的一种就是
结婚之后
把男生和女生的姓都放在一起
可能他有一个 middle name
就放在那个里面就是还是都有的
或者对或者有些女生他就不改了
就还是用他原来的姓
也没问题
是是对对对对
而且我觉得现在有一些中国人
如果比较开放的中国人还有一种可能
他家有几个孩子
第一个跟爸爸姓
第二个跟妈妈姓这种情况也是有的
对我觉得就是夫妻两个自己去商量嘛
如果他们都能接受的话就没什么
嗯对
对对所以也不是真的就是
很封建的很歧视女性的这种想法
哈哈哈对啊我觉得应该没有了
嗯嗯
对好那今天
也聊了很多啊
比我想的聊的长怎么说了这么久
哈哈哈哈哈哈啊
所以最后我想强调一点就是
我们可能有时候说的一些东西
都是我们自己的看法
不一定那么专业
嗯
但是我们的播客的目的其实就是交流
而不是说我是一个老师
来教你一些什么事情啊嗯
比如说
可能我今天录音录的这个东西
过了三年
过了五年之后
听我觉得挺愚蠢的
其实我有时候做播客哈哈哈对
我有时候做播客我会有这样的想法
就是我过几年
我都会觉得我今天聊的东西挺蠢的
我为什么我要做这件事
但是给大家一个平台去思考去想吧
就我可能提起这个话题
你可以有更多的思考
然后你也可以告诉我呀
比如说你有不同意的地方
你也可以写 email 告诉我你的想法
其实我更希望就是
有更多的人通过你的这个平台
在下面留言也好
跟我们发邮件也好
告诉一些我们不知道的事情
或者是我们
一些不正确的认知也好
或者太片面认知也好告诉我们
对对对
这样我觉得对
是啊是啊
有朋友这样做
就是有有人给我们写信吗
而且上一期
因为我不是讲了那个在我
澳大利亚的时候
我的不是电脑和钱被偷了吗
然后我提到那个人是马来西亚来的
然后就有一个听众给我写了 email 说
你不应该说是马来西亚来的人
针对性太强了
就好像觉得马来西亚人都是坏人
就是你只要提
有一个人偷了你的东西就好了
其实说的很对
我当时也没有想那么多
是因为我住的那个地方全是中国人
就他们是马来西亚人
我就提了一嘴
而且我也没有提我那里面全是中国人
所以别人听起来可能觉得我有点
好像你刻意指出
对就好像有点种族歧视这样的感觉
所以其实做播客
也就是言者无心听者有意就是这样
但是也应该注意因为呃播客听
众越来越多了
现在我们
已经有六十多个国家的人在听我们
胡说八道啊所以
哈哈哈所以慢慢的
人越来越多
你会有一点责任感就不可以
乱说会误导别人
对的
没错
嗯
所以我有时候做播客也是
（有）一点点矛盾
就可能我要特别认真的去做做准备
然后
说话应该要小心一点
但是平时我俩说话就是挺随意哈哈哈
对啊我就是说话特别不小心的人
希望大家可以原谅我
谢谢
哈哈哈哈哈
哎你太可爱了大家肯定会原谅你
哈哈哈哈哈哈
嗯希望大家也都是一个开放
和宽容的心态
如果我们说的有什么问题
你指出来告诉我们
我们也觉得是一个学
习是非常好的事情
所以对
嗯也谢谢大家听我们啰里八嗦又说了
这么长时间
那我们下次见啦
也谢谢 yifei 抽空和我录音
那下次见啦
好的谢谢大家
拜拜"""


subtitles3 : Generator (List Subtitle)
subtitles3 =
    generateSubtitles "8FKWqzd5jjs" """哈喽
大家好欢迎来到聊聊东西
我们聊东西文化也聊很多东西
我是 Candice
今天呢
想跟大家聊的是关于睡眠的话题
关于失眠和晚睡强迫症的
啊因为我也不是专业睡眠方面的医生
所以我肯定是从一个普通人的角度来谈这个问题
那首先呢
我个人觉得晚睡强迫症和失眠
它是有本质上的区别的
首先失眠是你很想睡
但是你睡不着
可能你很早就躺在床上
你九点十点就躺上去了
但是凌晨两三点还睡不着
那这个是失眠
那晚睡强迫症是
其实你困了
可能你到十一点十二点
比较晚的时间你已经想睡觉
有困意了
但是你逼着自己清醒
然后可能你刷手机啊看剧啊玩游戏啊
但就是不睡觉
那这两个有一点点不一样
所以我们一会分开来讨论
那为什么想讨论这个问题呢
因为最近看到一个数据
他显示的二零二零年就是两年前
大概这个时间十月份左右的时候
这个数据显示
全国有超过三亿人存在睡眠障碍的问题
说全中国大约有百分之八十四的九零后
存在睡眠障碍问题
就现在对年轻人
睡眠是一个很大的问题了
但是在以前可能没有这么明显
好那我们先来说失眠吧
我不知道大家都有没有失眠的经历啊
如果你失眠了你会怎么办
呃我记得也是大概两年前
我看节目
因为我很喜欢看窦文涛的节目
所以我看他的圆桌派
我记得有一集他就讲他是常年失眠的
他总是睡不着觉
然后他说有一次他试了一个方法
那段时间睡眠确实是好了
但是这个方法后来他就放弃了
因为太伤身体了
然后对第二天的工作也有影响
他就是喝红酒
他说刚开始他就是
每天晚上睡前喝一杯
然后你有点醉意正好去睡觉
到后来变成喝一瓶
就一天晚上喝一瓶他才能睡着觉
也就不是他真的睡着了
是因为这个酒麻痹了他的神经
他喝醉了他才睡着了
那这样第二天
他的精神状态肯定是很差的
然后如果他去主持节目的话
他整个脑袋转的也是比较慢的
失眠这个问题一直是他
很大的一个问题
那后面我也会讲
具体怎么样去解决失眠这个问题
我也讲一下我自己失眠的经历好了
啊
其实我小的时候完全没有失眠的问题
我可能到大学的时候都没有失眠的问题
都是那种
如果你给我时间
我可以从晚上睡到第二天中午
但我觉得年轻人一般都是这样
年轻的时候你睡觉比较多
然后随着年龄慢慢增长
你的睡眠时间确实是会下降的
以前的睡眠特别好
打雷啊闪电
就是很大声音下雨什么的
我都不会醒
但我开始严重失眠的
在澳大利亚的时候
呃因为那个时候我跟一群陌生人一起合租房子
就是我刚到澳大利亚不久
我刚去的时候是住的那个 airbnb
那个时候我是住在一个警察局的旁边
就是真的旁边
我打开窗户望出去就是警察局
那个时候住的觉得特别特别安全
但是我每天住 airbnb 太贵了
我不可能一直长期住在那
所以后来就搬家了
我不记得我以前有没有讲过这段经历
就我搬家
是因为我正好认识了两个朋友
都是住在那个附近
然后我就在那附近呃找房子
在网上看房子
我当时的想法也是觉得
可能找一个中国人的房东
会比较好沟通嘛
所以就找了中国人的房东
然后室友都是陌生人就是
完全没有见过面的
但是大家都住在啊一个房子里
共用可能客厅啊
厕所啊这样的东西
然后我就找到房子就过去住了
其实这个房子我后来才发现
一点也不便宜
就我当时可能跟我的 airbnb 比
我觉得还挺便宜的
但后来再看一下这个市场上的价格
反正挺贵的
住了前面几个月反正也没什么大问题
除了那个澳大利亚房子隔音比较差
呃会有点影响你睡觉
就感觉别人或者走来走去
或者是年轻人吗
我跟一些年轻人一起住
他们喜欢晚上熬夜
晚上比较吵
也会听到他们的声音
但这个都不算是最大的问题
忽然有一天
在我准备交房租给房东的时候
发现我的钱少了 100 块澳币
呃然后我有自己想一想
是不是可能我自己花掉了
然后不记得了
但是我觉得我不可能
因为那个时候没有没有很多钱
我会提前把我的那个房租准备好
放在我的呃钱包的后面一格
然后前面一格就放我平时要用的钱
所以我就在想为什么我的钱会不见
然后不知道应该怎么做
然后我就跑到另一个小妹妹的房间
那个大学生的房间
跟他说这个事
然后那个时候我们是站在走廊里说这件事的
跟我说他好像也
发现以前他有不见过钱
但是他也没多想
应该不会有人真的偷钱
我也是怀疑我也怕自己啊
记错了或者会冤枉人什么的
但是他也说他不见过
所以我就觉得有可能
是不是真的这个房子里有小偷
因为他在这住了很久吗
我就问他这个房子里有些什么人
然后他就说除了其他人都是学生之外
有一个是两个人
他们是在附近的越南餐厅打工的
好像是马来西亚人吧
可能因为我们在走廊里
说这件事然后隔音又不是很好吗
别人都听得见这个小偷
可能他就自己听见了
然后当时我俩也是有点激动
说的很气愤的样子
就说如果找到这个小偷一定要去呃警察局啊什么之类的
在那之后的晚上
我就开始总是听到那种声音
总是有那种奇奇怪怪的声音
好像开我的窗户
因为他那个窗户如果你打开大点
大一点的话
是可以从外面进来的
我又不可能一直把窗户锁着
因为他的窗户没有那种纱窗
很老的房子
就只有那种玻璃推开的窗户
所以我一般晚上都是开一条缝睡觉的
然后我又跟那个妹妹说
我说我这几天晚上总是听到声音
然后他就跟我说啊
可能是那个果子狸
他说就是那种动物
因为在后院的树上
他们常常就是晚上会出来活动
他说他有时候晚上在他窗户那
都看到过果子狸
我就想好那
可能又是我想多了不会是什么小偷
就是那动物吧
但是晚上开始总是很容易醒
很容易听到声音然后醒
不容易睡着
然后突然有一天我回家之后
我的电脑不见了
只剩充电线在那
然后电脑不见了
这就明显是小偷了呀
而且他偷我电脑干嘛呢
这电脑能卖几个钱
而且那个时候
那个电脑已经用好多年了
一个苹果的那个小电脑
我当时非常生气
我就直接跟房东说了
我说现在发生这种事情了怎么办吧
然后房东不是很相信我你知道吧
给我发信息说了个什么
类似就是如果你不想住在这
你可以直接说
不要用这种借口
我特别生气我就直接报警了
那可能是我有史以来第一次报警
竟然是发生在澳大利亚
我在中国都没有报过警
啊报警的时候警察其实挺快就来了
来了我记得有一个男警察一个女警察
我就不知道有什么好笑的
我在跟他解释这个情况的时候
他们俩总是相视一笑
是觉得这个事情可能很荒谬
然后第二天就另外的警察过来
在窗户啊门把手啊这里来扫指纹啊
做一些检查
那是我第一次看到啊
就是警察怎么来就是解决这些案件的
然后也没有什么发现
没有任何有意思的发现
但是就在警察来的第二天
来取证的第二天
在越南餐厅打工的那一对情侣
那个室友他们就搬走了
而且房东说他们连押金都没要
直接就搬走了
这还不明显吗
就是小偷呗
怕查到他们就连夜搬走了
连押金都不要了
但我电脑怎么办
就是
真的很庆幸我买的是苹果电脑
就我手机也是苹果手机
好像那功能叫 find my iphone 吧
就是反正
我可以在我的手机上点一个什么东西
然后就是可以找到我的电脑
然后并且可以警告另一个人吧
反正我就是找到了我的电脑在的地方
就根据那个定位找到了
然后我一个女生朋友真的胆子特别大
他开着车带着我
呵呵然后就去了那个地方
但是我俩也不敢冲进去啊
是特别特别搞笑的一个经历
但是我们知道电脑就在那
然后就在我们家附近另外一个房间
可能
那两个人搬到一个很近的另外的地方
啊我们也不敢去
我也不敢拿他的生命冒险
毕竟只是一个电脑
然后还有块钱
然后最后就又回去了
不了了之了
过了几天我和另外一个朋友
我还记得特别清楚
那个时候我们在那个嗯
市中心吃 brunch
然后好像是还是吃的什么巧克力
呃网红的店的巧克力
然后我突然接到电话
是警察打来的说找到我的电脑了
让我去警警察局拿
我真的是这种失而复得的心情
没有人能知道
然后那个时候
我也不想让我的爸爸妈妈担心
我也没有跟他们说这件事
但是我就在我的朋友圈发了一条
想要求安慰吧
那个时候可能想要别人来安慰我
但是
屏蔽了我的爸爸妈妈和一些亲朋好友
那个时候
反正就把我的经历简单的说了一下
然后特别感动的是
呃我以前的一个学生的家长
其实我们也不是特别熟
然后他看到我的朋友圈之后
就给我微信发了信息
说
需要帮助吗
如果需要钱就是跟他讲他会帮我
我当时真的特别感动
我现在都记得那个时候的感觉
当然我不会要他钱
我也很感谢他
我不知道他是出于一个什么样的心态
可能他想到
如果是他自己的孩子
在国外遇到这样的事情
也会需要帮助吧可能
好这个经历说的实在太长了
但其实我就是想说
从这个经历之后我就开始失眠
啊对这个晚上对声音特别特别敏感
非常容易醒
然后花了很多很多的时间慢慢的恢复
我不知道大概这个事情发生几年之后
我才慢慢的就是没有那么敏感
睡觉没有那么敏感
所以我完全知道失眠
是一件多么可怕的事情
因为你头一天晚上很精神紧张
也没睡好
第二天会影响工作影响心情
而且我那个时候都没有去想
去试一些办法
晚上睡觉的时候听到声音就特别害怕
就总觉得
那个小偷又回来要偷我的东西
或者想要报复我
如果听到有人进来的时候我就
其实是听到了我不敢动你知道吧
就是拿被子把自己裹得严严的
然后使劲的闭上眼睛
那个时候我都没有去看心理医生
看不起在澳大利亚我觉得很贵
哈哈
所以我也没有想这件事情
就是慢慢的自己好起来的
好那我们再说一下这个晚睡强迫症
啊晚睡强迫症我也有哈
哈这两个经历我都有
我到现在我都觉得我有点晚睡强迫症
我就是那种
晚上有困意但是还强迫自己不睡
要做点什么乱七八糟事情的
我觉得最主要的原因还是
白天的时候你打工
你搬砖
你做了很多事情都不是你自己想做的
或者不是你的私人时间
那到了晚上你终于觉得啊
这是我自己的私人时间了
我终于可以自己来掌控了
我想要放松一下看一下视频
或者是我想要学习一下
或者是怎么样我都觉得我舍不得睡
这个时间我好像还要再利用一会再睡
我好像才比较划算
就导致了晚睡强迫症
但这个事
也好像是很多年轻人会遇到的
嗯一个问题
虽然他和失眠的原因不一样
但是他导致的结果可能是一样的
就是第二天你们没有精神
然后感觉很累
那我又说回刚刚窦文涛那个主持人
就是我看了他那个节目的另外一个
年纪比较大的嘉宾
说了另外一个观点
我不太记得他的名字了
因为时间有点久
就是他提到说
其实有时候你睡着了
但是你以为你没睡着
因为你没有进入深度睡眠
就是在浅睡眠的时候可能你
朦朦胧胧的那种感觉
你以为你没睡着
但其实那也是休息
只要你把眼睛闭着了
你的身体没有什么消耗
你也在休息
只是说没有深度睡眠休息的那么好
我其实挺同意的
你不要把睡眠这件事情看得太重
你看的越重越紧张你就越睡不着觉
你越告诉自己快睡快睡快
睡越是睡不着
啊我最近还看另外一个节目
就是张朝阳呃请那个
新东方的俞敏洪
一起做了一个直播节目
直播了五个小时
我是没有时间和心情看五个小时直播
但是我看了一些片段精华
就是有一些网友
选了一些比较精华的对话
然后放在网上
我看到他提到的一个就是分段式睡眠
他本人就是他说他每天睡五个小时
分两段睡
第一段可能睡两个小时
第二段睡三个小时
就大概
大概这个样子
他说这两段睡的话肯定都是深度睡眠
而且效率特别高
虽然我不知道这个科不科学
我也不想这样尝试
但是我的意思就是说
不要用传统的睡眠概念去束缚自己
不要一直想着好像啊
成年人每一天就是要睡六到八个小时
要从晚上十二点睡到白天六点
什么之类之类的
不一定要规定的那么死
就是如果你很累的话你中午
在那个办公桌上趴个十分钟十五分钟
小睡一会
也会对你下午的工作有很大的
那个精力的提升
现在年轻人啊都睡不着觉
三亿人有失眠问题啊
所以导致了现在这个
睡眠经济蓬勃发展
就是看到了这个当代年轻人的痛点吗
尤其是年轻人
有很多不一样的睡眠产品对吧
最直接的可能就是如果你去找医生
医生会给你安眠药
但我个人是没有吃过安眠药的
或者是那种保健品类的褪黑素之类的我是没有吃过的
哦 我吃过那种澳洲药房里面卖的那种帮助睡眠的保健品
但是我不太清楚它具体成分是什么
对就是这么无知
当年呵呵乱吃保健品
那就是睡不着吗
就买这个希望自己能睡的更好
我觉得并没有什么用处
把那个东西吃了半瓶吧
然后后来就扔了过期了就扔掉了
就中国人那种骨子里的想法
还是觉得吃药对身体不好
吃多了对你的肝脏
内脏啊什么都有一定的影响
让他们的负担很大
不到万不得已不会去尝试的一种方法 吃药
然后第二种
这几年特别特别时兴的不就是那个 ASMR
那个的兴起让我觉得非常有意思
我哎
我想起来我以前用过那一种助眠的 APP 那种软件
就是里面有很多大自然的声音
比如说下雨声或者是海浪声
树林里的鸟叫声之类的可以帮你睡觉
我觉得好像我用的最多的是下雨声和海浪声
能不能帮你睡觉我不确定
但是可以让你有一点平静下来
以前用过
失眠的小伙伴可以试一下
这个 ASMR 我就不是特别清楚
我看的比较少
对我还想起来我越南
越南一个学生还跟我提到
就是有那种直播睡觉的
我到现在还没看
我一直说我应该找来看一下
为什么有人去看直播睡觉
他给我截了一个图
那个女生她也不是说穿的很少那种擦边
也没有穿的很少
就是正常的穿
然后正常的躺在床上
对 学生告诉我就是看着她睡觉
一直直播到第二天早上她起床
我觉得哇塞
这是什么方式都有
很感兴趣的可以看一下啊
然后我还看到那个在橙色软件上有那种陪聊
就也不是太贵
晚上睡觉前
可能有一个陌生人跟你聊天
陪你聊一聊放松一下然后睡觉
我觉得这种会让我更睡不着
我跟一个陌生人聊天
我还得打起精神来陪他聊
万一他聊呢
特别无聊
特别耗费我的精力
对我来说
我绝不可能尝试这种陪聊
然后再就是一些物理方面的
比如说那种床啊什么身
根据你的身体的结构制作的那种
然后我自己是很在乎枕头的
就是枕头如果太高或者太矮
我都睡不着
就我现在用的就是那种记忆枕
嗨 不过我觉得也是一种噱头吧
什么记忆枕
就是宣传可以记住你的颈椎和你的脑袋那个形状吗
就比较贴合
觉得也就是那回事
但是他这个高度和软硬程度
对我来说很合适
所以我也一直在用这个枕头
然后还有些人会用眼罩吧
我个人是不喜欢用眼罩的
感觉戴着不舒服
我不是很怕光比较亮的情况
如果我困的话我也可以睡着
所以我不知道大家还有试过别的
什么有意思的方式吗
也可以留言评论告诉我
嗯反正失眠这个问题
现在已经很好了
然后晚睡强迫症
这个我有一个特别好的办法就是
对我来说啊
但是对你们可能不一样
看书
但真的不是因为书无聊
然后我就很容易睡着
是因为书很有意思
然后我因为看了太久
我的大脑一直在思考和输入
我会觉得很累
如果我晚上看一个小时书
我会很早觉得很累想睡觉
再一个就是不要把你的手机带上床
虽然说这个比较难
可以试一下
但最重要我觉得是
不要把睡眠看得太重
如果没有严重的影响第二天的工作
生活的话
就没有问题
我觉得睡得晚一点只是说第二天累一点
但是第二天可能很早你可以睡着
所以我觉得最重要就是放轻松
睡觉很重要休息很重要
健康很重要
不要把睡眠看得太重
你就会更开心一点
好那我们今天
我一个人
噼里啪啦不知道怎么说了这么多
下次见了
下次可能是我一个人也可能有嘉宾
我还不知道因为一个月一次吗
我现在还没有计划好
那我们下次见啦拜拜"""