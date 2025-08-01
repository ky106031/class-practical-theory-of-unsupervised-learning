# install.packages("igraph")
library(igraph)

g <- make_graph("Zachary") # Zacharyの空手クラブの友人ネットワーク
plot(g) #実際にはplot.igraph()が呼び出される。

# igraphオブジェクト
g

# 頂点の数
vcount(g)

# 辺の数
ecount(g)

# このグラフの頂点集合
V(g)

# このグラフの辺集合
E(g)

set.seed(1234)
plot(g)

set.seed(1234)
plot(g, vertex.color="skyblue",vertex.label.color="black",vertex.label.cex=0.7,vertex.frame.color=NA)

set.seed(1234)
plot(g, main="default") # デフォルトでlayout_nicelyが使われている。

set.seed(1234)
l=layout_with_fr(g) # Fruchterman-Reingold配置アルゴリズム
plot(g,layout=l, main="Fruchterman-Reingold")

set.seed(1234)
l=layout_with_kk(g) # Kamada-Kawai配置アルゴリズム
plot(g,layout=l, main="Kamada-Kawai")

set.seed(1234)
l=layout_in_circle(g) # 円環上に配置
plot(g,layout=l, main="circle")

set.seed(1234)
l=layout_as_tree(g) # ツリー状に配置
plot(g,layout=l, main="tree")

set.seed(1234)
l=layout_with_drl(g) # DrL配置アルゴリズム
plot(g,layout=l, main="drl")

# デフォルトでは有向グラフ
adj <- matrix(c(0,1,1,0,0,1,0,0,0),nrow=3)
print("有向グラフ")
print(adj)
g <- graph_from_adjacency_matrix(adj)
set.seed(1234)
plot(g, main="有向グラフ")

# 無向グラフの時は、mode="undirected"
adj <- matrix(c(0,1,1,1,0,1,1,1,0),nrow=3)
print("無向グラフ")
print(adj)
g <- graph_from_adjacency_matrix(adj,mode="undirected")
set.seed(1234)
plot(g, main="無向グラフ")

# weightedを指定しないと、重み付き隣接行列を与えても辺の数自体が増える
adj <- matrix(c(0,2,1,2,0,3,1,3,0),nrow=3)
print("重み付き無向グラフ")
print(adj)
g <- graph_from_adjacency_matrix(adj,mode="undirected")
set.seed(1234)
plot(g,main="weighted未指定")

# weightedを指定すると、重み付き隣接行列から重み付きグラフが作られるが、デフォルトのプロットでは重みは表示されない。
g <- graph_from_adjacency_matrix(adj,mode="undirected",weighted=T)
set.seed(1234)
plot(g,main="weighted=TRUE")

# グラフの辺集合のweightをedge.labelに指定
set.seed(1234)
plot(g, edge.label = E(g)$weight,main="辺に重みを表示")

# さっきのグラフ
g <- make_graph("Zachary") # Zacharyの空手クラブの友人ネットワーク
set.seed(1234)
plot(g)

# このグラフから辺リストを取得
el <- as_edgelist(g)
print(el)

# 辺リストからグラフを作るとデフォルトでは有向グラフ
g <- graph_from_edgelist(el)
set.seed(1234)
plot(g)

# 辺リストから無向グラフを作る
g <- graph_from_edgelist(el,directed = F)
set.seed(1234)
plot(g)

el <- read.table("out.moreno_zebra_zebra", comment.char = "%")
print(el)

g <- graph_from_edgelist(as.matrix(el),directed = F)

# こちらでもよい
# g <- graph_from_data_frame(el,directed = F)

set.seed(1234)
plot(g)

el <- read.table("aves-barn-swallow-contact-network.edges", comment.char = "%", col.names=c("from", "to", "weight"))
# 3行目は重み
print(el)

g <- graph_from_data_frame(el,directed = F)
set.seed(1234)
plot(g,edge.label = E(g)$weight)

el <- read.table("facebook_combined.txt", comment.char = "%")
print(el)

g <- graph_from_data_frame(el,directed = F)

set.seed(1234)
plot(g,vertex.size=1, vertex.label=NA, edge.width=0.1,vertex.frame.width=0.1,)

set.seed(1234)
g <- sample_gnp(100,0.1)
plot(g,vertex.size=5, vertex.label=NA, edge.width=0.1, main="n=100")

set.seed(1234)
g <- sample_gnp(1000,0.01)
layout <- layout_with_fr(g)
plot(g,vertex.size=2, vertex.label=NA, edge.width=0.1, layout=layout,vertex.frame.width=0.1, main="n=1,000")

set.seed(1234)
g <- sample_gnp(10000,0.001)
layout <- layout_with_fr(g)
plot(g,vertex.size=1, vertex.label=NA, edge.width=0.1, layout=layout,vertex.frame.width=0.1, main="n=10,000")

set.seed(1234)
g <- sample_smallworld(dim=1,size=100,nei=2,p=0.1)
layout <- layout_with_fr(g)
plot(g,vertex.size=5, vertex.label=NA, edge.width=0.1,layout=layout, main="n=100")

set.seed(1234)
g <- sample_smallworld(dim=2,size=30,nei=1,p=0.1)
layout <- layout_with_fr(g)
plot(g,vertex.size=2, vertex.label=NA, edge.width=0.1, layout=layout,vertex.frame.width=0.1, main="n=900")

set.seed(1234)
g <- sample_pa(n=100,power=1,directed=F)
plot(g,vertex.size=5, vertex.label=NA, edge.width=0.1, main="n=100")

set.seed(1234)
g <- sample_pa(n=100,power=1.5)
plot(g,vertex.size=5, vertex.label=NA, edge.width=0.1,edge.arrow.size=0.1, main="n=100")

set.seed(1234)
g <- sample_pa(n=1000,power=1.1,directed=F)
layout <- layout_with_fr(g)
plot(g,vertex.size=2, vertex.label=NA, edge.width=0.1, layout=layout,vertex.frame.width=0.1,edge.arrow.size=0.05, main="n=1,000")

set.seed(1234)
g <- sample_pa(n=10000,power=1.1,directed=F)
layout <- layout_with_fr(g)
plot(g,vertex.size=1, vertex.label=NA, edge.width=0.1, layout=layout,vertex.frame.width=0.1,edge.arrow.size=0.05, main="n=10,000")

# ここでは、irisデータを使う
iris

# irisデータ全体の特徴量間の相関行列
rho <- cor(iris[,-5])

# 隣接行列を作成
a <- rho>0.5 # 相関係数が0.5以上を残す。
diag(a) <- 0 # 対角要素を0に

# 隣接行列からグラフを作成
g <- graph_from_adjacency_matrix(a, mode = c("undirected"))
set.seed(1234)
plot(g)

# setosa種の特徴量間の相関ネットワーク
rho <- cor(iris[iris$Species=="setosa",-5])
a <- rho>0.5
diag(a) <- 0
g <- graph_from_adjacency_matrix(a, mode = c("undirected"))
set.seed(1234)
plot(g, main="setosa種")

# versicolor種の特徴量間の相関ネットワーク
rho <- cor(iris[iris$Species=="versicolor",-5])
a <- rho>0.5
diag(a) <- 0
g <- graph_from_adjacency_matrix(a, mode = c("undirected"))
set.seed(1234)
plot(g,main="versicolor種")

# virginica種の特徴量間の相関ネットワーク
rho <- cor(iris[iris$Species=="virginica",-5])
a <- rho>0.5
diag(a) <- 0
g <- graph_from_adjacency_matrix(a, mode = c("undirected"))
set.seed(1234)
plot(g,main="virginica種")

g <- make_graph("Zachary") # 最初に使ったグラフ
degree(g)
set.seed(1234)
plot(g,vertex.size=degree(g))

betweenness(g)

# 特定のノードの媒介中心性が極端に大きく、可視化としてはうまくいかない。
set.seed(1234)
plot(g,vertex.size=betweenness(g))

# 例えば、対数変換してみた例
set.seed(1234)
plot(g,vertex.size=log(betweenness(g)+1))

#edge_betweenness(g)

set.seed(1234)
plot(g,edge.size=1)
set.seed(1234)
plot(g,edge.width=log(edge_betweenness(g)+1))

res <- cluster_optimal(g)

# 選ばれたコミュニティ構造のモジュラリティ指標
modularity(g, res$membership)

# igraphの機能に任せたコミュニティ分割の可視化
# "generic"の機能により、plot.igraph()ではなくplot.communities()が呼び出される。
set.seed(1234)
plot(res,g)

# 手動でのコミュニティ分割の可視化
set.seed(1234)
plot(g,vertex.color=res$membership)

res <- cluster_edge_betweenness(g)

# 選ばれたコミュニティ構造のモジュラリティ指標
modularity(g, res$membership)

# 分割されていく過程のデンドログラム
plot(as.dendrogram(res))

# 分割過程のモジュラリティ指標
plot(res$modularity)

# 選ばれたコミュニティ分割
set.seed(1234)
plot(res,g)

res <- cluster_louvain(g)

# 選ばれたコミュニティ構造のモジュラリティ指標
modularity(g, res$membership)

# 選ばれたコミュニティ分割
set.seed(1234)
plot(res,g)

res <- cluster_leiden(g)

# 選ばれたコミュニティ構造のモジュラリティ指標（デフォルトのパラメータではなぜかうまくいっていない）
modularity(g, res$membership)

# 選ばれたコミュニティ分割
set.seed(1234)
plot(res,g)

# コミュニティ分割（resolutionパラメータを調整）
res <- cluster_leiden(g, resolution=0.1)
set.seed(1234)
plot(g,vertex.color=res$membership)

# 選ばれたコミュニティ構造のモジュラリティ指標
modularity(g, res$membership)

# 選ばれたコミュニティ分割
set.seed(1234)
plot(res,g)
