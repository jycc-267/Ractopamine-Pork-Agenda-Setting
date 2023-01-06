# NLP Analytics on Agenda Setting Behaviors of Partisan Media - The Case of 2021 Taiwanese Referumdum on "Ractopamine Pork Issue"
從是否開放萊克多巴胺肉品十多年來的政策脈絡，政治圈普遍可以總結兩項直觀看法：其一，萊豬進口議題的本質就是高度政黨對決，兩黨立場會隨著執政與否浮動，而非長期對議題本身如食品安全、國際參與的價值信仰；其次，2020年在總統大選大敗後的藍營，除了組織動員能力不如民進黨，甚至在空戰，即傳統媒體以及社群媒體宣傳上，也遜色不少。

以上兩項觀察，應是任何關心臺灣政壇的人，和政治圈內人士都會認同的直觀看法。前者在本地政治學研究中，有不少關於地方勢力與政黨競合的選舉研究可供參考及驗證，甚至若能訪談到身居要津的選舉操盤手，即可能獲得可供驗證的第一手資料。然而就後者來看，無論傳播學還是政治學，目前針對黨性媒體（partisan media），或政黨與媒體關係的實證研究較少，難有既定框架或合適的研究方法得呈現空戰的強弱，更難以推論2021/12/18號萊豬公投案所帶來的政策變遷結果與空戰表現優劣的關係。本專案嘗試透過資料科學的文字分析途徑（text mining approach）諸如詞袋模型（bag-of-words）、主題模型分布（topic model）、共現詞等（word co-occurrance／collocation），分析傳播學聚焦新聞媒體的議題設定行為，進而推論政策變遷理論「倡導聯盟架構」（Advocacy Coalition Framework）中泛藍泛綠媒體雙方在論述上的資源配置，是否符合各自最適當的議題設定效果。

There are two intuitive views which can be summarized in the politics circle regarding whether the Taiwanese government should open the import of meats with ractopamine. First, the essence of this issue is a partisan confrontation. It is less about long-term values such as supporting food safety or international participation. Second, the opposition party Kuomintang's (KMT) ability to mobilize its supporters declined (both traditional media and social media propaganda) after the defeat in the 2020 presidential election.

We already have abundant research in Election Studies and Political Cleavage addressing the first opinion; however, these political parties have failed to measure how well do partisan media present in the process of an election due to the lack of research and emperical evidence. This project aimed to provide a analystic framework using text mining approaches including bag-of-words, word to vector, collocation, and topic model. We want to observe whether the real distribution of media's propaganding resources aligns with their optimal agenda-setting strategies during the 2021 referendums. We compared the agenda-setting behaviors between two “pan-blue” and two “pan-green” media in Taiwan (*“Blue” represents Kuomintang and “Green” represents Democratic Progressive Party), and discovered that the two “pan-green” media were more unanimous than the two “pan-blue” media on the wordings and topics of coverage related to “Ractopamine Pork”. The outcome of this referendum at the end of 2021 supported the result.


- [爬蟲與資料前處理(Data Retrival and Preprocessing)](preprocessing)
- [文字模型程式碼(Text Mining Models)](model)
- [投影片(Presentation)](presentation.pdf)
- [書面報告(Report)](writing_sample.pdf)
- [使用資料(Raw Data)](data)
- [視覺化(Visualization)](plot)
