java -jar RankLib-2.1-patched.jar -train "..\data\%1.l2r" -validate "..\data\%2.l2r" -ranker 1 -metric2t NDCG@38 -norm zscore -save "..\artifacts\ranklib-RankNet-model-%3.mdl"
