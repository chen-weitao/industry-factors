data(data)
factor.mat = data[1:342,2:4] # Fama-French 3-factor model
ret.mat = data[1:342,8:ncol(data)] # 25 size-BM portfolio returns
GRS.test(ret.mat,factor.mat)$GRS.stat # See Table 9C of Fama-French (1993)