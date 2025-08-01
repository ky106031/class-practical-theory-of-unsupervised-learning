# データの読み込み。ここでは、ダウンロードしたファイルから必要な行と列を抽出したファイルを読み込んでいる。
df <- read.csv("SSDSE-C-2023_formatted.csv",row.names = 1)

## 生データをプロット
heatmap(as.matrix(df),main="default") 
# デフォルトで、行と列のそれぞれがクラスタリングされる。
# また、行ごとに正規化される

heatmap(as.matrix(df),scale="row",main='scale="row"')
heatmap(as.matrix(df),scale="column",main='scale="column"')
heatmap(as.matrix(df),scale="none",main='scale="none"')
heatmap(as.matrix(df),scale="column",Rowv=NA,main="行のクラスタリングなし")
heatmap(as.matrix(df),scale="column",Rowv=NA,Colv=NA,main="行、列のクラスタリングなし")

## 列に対する相関行列
corr <- cor(df)
print(round(corr,2))

## 列に対する相関行列をプロット
heatmap(corr,scale="none",main='scale="none"')
heatmap(corr,symm=T,main="symm=T") # 相関行列プロット時はsymm=Tとした方がみやすく便利

## 行に対する相関行列
corr <- cor(t(df))
print(round(corr,2))

## 行に対する相関行列をプロット
heatmap(corr,symm=T)
heatmap(corr,symm=T,Rowv=NA,main="クラスタリングなし")

res <- prcomp(df,scale.=T)

# グラフを正方形に表示するおまじない
par(pty="s")

# 正規化を行わず主成分分析
res <- prcomp(df)

# 第１、第２主成分プロット
plot(res$x[,c(1,2)]) 

# 正規化を行なって主成分分析
res <- prcomp(df,scale.=T)

# 第１、第２主成分プロット
plot(res$x[,c(1,2)]) 

# 第１、第３主成分をプロット
plot(res$x[,c(1,3)])

# 第１、第２主成分を行名でプロット
# 最初にtype="n"でプロットを行うと、点は表示されず、軸のみ描画される。
# その後、text関数で主成分得点の位置に都道府県名（行名）を表示している。
plot(res$x[,c(1,2)],type="n")
text(res$x[,c(1,2)],rownames(df))

# 第１、第２主成分のバイプロット
biplot(res,choices = c(1, 2),cex=0.5)

# 重みベクトルの可視化
rot <- res$rotation[,1]
o <- order(rot,decreasing = T)
# lasオプションでラベルの向きを縦にしている。
barplot(c(rot[o[1:10]],rot[o[(length(rot)-10):length(rot)]]),las=2,main="第１主成分の重みベクトル")

rot <- res$rotation[,2]
o <- order(rot,decreasing = T)
barplot(c(rot[o[1:10]],rot[o[(length(rot)-10):length(rot)]]),las=2,main="第２主成分の重みベクトル")

rot <- res$rotation[,3]
o <- order(rot,decreasing = T)
barplot(c(rot[o[1:10]],rot[o[(length(rot)-10):length(rot)]]),las=2,main="第３主成分の重みベクトル")

summary(res)

pca_summary <- summary(res)
barplot(pca_summary$importance[2,],las=2)
barplot(pca_summary$importance[3,],las=2)

heatmap(cov(res$x),symm=T,Rowv=NA)

#install.packages("Rtsne")
library(Rtsne)

par(pty="s")
set.seed(1234)
# ここでは、主成分分析を行わず、また、theta=0として厳密なt-SNEを行っている。
tsne <- Rtsne(df,pca=F,normalize=T, perplexity=15,theta=0)
plot(tsne$Y,xlab="t-SNE1",ylab="t-SNE2")

# 地域ごとに色指定
# palette = "Okabe-Ito"は、色覚多様性に配慮した色を生成してくれる（らしい）
cols=palette.colors(palette = "Okabe-Ito")
col_map=c(cols[1],rep(cols[2],6),rep(cols[3],7),rep(cols[4],9),rep(cols[5],7),rep(cols[6],5),rep(cols[7],4),rep(cols[8],8))
plot(tsne$Y,xlab="t-SNE1",ylab="t-SNE2",type="n")
text(tsne$Y,rownames(df),col=col_map)

par(pty="s")

set.seed(1234)
tsne <- Rtsne(df,pca=F,normalize=F, perplexity=15,theta=0)
plot(tsne$Y,xlab="t-SNE1",ylab="t-SNE2",type="n",main="主成分分析なし、規格化なし")
text(tsne$Y,rownames(df),col=col_map)

set.seed(1234)
tsne <- Rtsne(df,pca=F,normalize=T, perplexity=15,theta=0)
plot(tsne$Y,xlab="t-SNE1",ylab="t-SNE2",type="n",main="主成分分析なし、規格化あり")
text(tsne$Y,rownames(df),col=col_map)

par(pty="s")

set.seed(1234)
tsne <- Rtsne(df,pca=T, pca_scale=F, normalize=F, perplexity=15,theta=0)
plot(tsne$Y,xlab="t-SNE1",ylab="t-SNE2",type="n",main="主成分分析前正規化なし、t-SNE前正規化なし")
text(tsne$Y,rownames(df),col=col_map)

set.seed(1234)
tsne <- Rtsne(df,pca=T, pca_scale=T, normalize=F, perplexity=15,theta=0)
plot(tsne$Y,xlab="t-SNE1",ylab="t-SNE2",type="n",main="主成分分析前正規化あり、t-SNE前正規化なし")
text(tsne$Y,rownames(df),col=col_map)

set.seed(1234)
tsne <- Rtsne(df,pca=T, pca_scale=F, normalize=T, perplexity=15,theta=0)
plot(tsne$Y,xlab="t-SNE1",ylab="t-SNE2",type="n",main="主成分分析前正規化なし、t-SNE前正規化あり")
text(tsne$Y,rownames(df),col=col_map)

set.seed(1234)
tsne <- Rtsne(df,pca=T, pca_scale=T, normalize=T, perplexity=15,theta=0)
plot(tsne$Y,xlab="t-SNE1",ylab="t-SNE2",type="n",main="主成分分析前正規化あり、t-SNE前正規化あり")
text(tsne$Y,rownames(df),col=col_map)

set.seed(1234)
tsne <- Rtsne(df,perplexity=15,theta=0)
plot(tsne$Y,xlab="t-SNE1",ylab="t-SNE2",type="n",main="デフォルト")
text(tsne$Y,rownames(df),col=col_map)

# install.packages("umap")
library(umap)

par(pty="s")

set.seed(1234)
res_umap <- umap(df)
plot(res_umap$layout,xlab="umap1",ylab="umap2")

# umap.defaultsにumapでデフォルトで利用されるパラメータが格納されている。変更したければ、umap関数を呼び出すときにこれらのパラメータ名をオプションとして設定する。
print(umap.defaults)

par(pty="s")

# データを正規化せずにUMAP
set.seed(1234)
res_umap <- umap(df,n_neighbors=10)
plot(res_umap$layout,xlab="umap1",ylab="umap2",type="n")
text(res_umap$layout,rownames(df),col=col_map)

par(pty="s")

# scale関数で事前に正規化を行った例
set.seed(1234)
res_umap <- umap(scale(df))
plot(res_umap$layout,xlab="umap1",ylab="umap2",type="n")
text(res_umap$layout,rownames(df),col=col_map)
