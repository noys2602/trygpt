rm(list=ls()) # del all objects and functions
gc() #cleans memory
options(scipen=999) # tell R not to use Scientific notation
options(digits = 5) # controls how many digits are printed by default
options(na.action=na.exclude)

library(openxlsx)

setwd("/Users/noys/Desktop/Noy_Code/data_macro_analitics")

# reading the data with its names
data1 = read.xlsx("SAM_input.xlsx","table1")
data2 = read.xlsx("SAM_input.xlsx","table2")
ind_names=read.xlsx("SAM_input.xlsx","names")
rownames(data1)=data1$names
data1=data1[,-1]
rownames(data2)=data2$names
data2=data2[,-1]

# creating the SAM's names vector
ind_143 = rownames(data1)[1:143]
com_143 = paste0("com",seq(1,143))
rest = c("tax","sub","gov","k","l","HH","IS","totex","im1","im2","cif","mtt","importtax","vat","income_tax")
names = c(ind_143 , com_143 , rest)

# creating the empty SAM
sam_table <- data.frame(matrix(nrow = length(names), ncol = length(names)))
rownames(sam_table) <- names
colnames(sam_table) <- names

# inserting data2 in the SAM
sam_table[com_143,ind_143]=data2[ind_143,ind_143] #b3
sam_table["tax",ind_143]=data2["Production_Taxes",ind_143] #b4
sam_table["sub",ind_143]=data2["DomesticProduction_Subsidies",ind_143] #b5
sam_table["k",ind_143]=data2["Other value added",ind_143] #b7 
sam_table["l",ind_143]=data2["Compensation for jobs",ind_143] #b8
sam_table[com_143,"gov"]=data2[ind_143,"Government_Collective"]+data2[ind_143,"Government_Individual"]
sam_table[com_143,"HH"]=data2[ind_143,"PrivateConsumptionHH"]+data2[ind_143,"PrivateConsumptionOther"]
sam_table[com_143,"IS"]=data2[ind_143,"Investments_FixCapital"]+data2[ind_143,"Investments_Changes"]
sam_table[com_143,"totex"]=data2[ind_143,"Export_Goods"]+data2[ind_143,"Export_TouristsConsumption"]

# inserting data1 into the SAM
sam_table[ind_143,com_143]=data1[ind_143,ind_143] #c2
sam_table["im1",com_143]=data1[ind_143,"Import_Competative"] #c12
sam_table["im2",com_143]=data1[ind_143,"Import_Complementar"] #c13
sam_table["cif",com_143]=data1[ind_143,"CIF_FOB"] #c14
sam_table["mtt",com_143]=data1[ind_143,"TradeTransportMargin"] #c15
sam_table["importtax",com_143]=data1[ind_143,"Import_Taxes"] #c16
sam_table["vat",com_143]=data1[ind_143,"DomesticProducts_Taxes"] #c17

# exo
sam_table["IS","gov"]=30000

# calculating
sam_table["gov","tax"] = sum(sam_table["tax",ind_143],na.rm = T)
sam_table["gov","sub"] = sum(sam_table["sub",ind_143],na.rm = T)
sam_table["gov","importtax"] = sum(sam_table["importtax",com_143],na.rm = T)
sam_table["gov","vat"] = sum(sam_table["vat",com_143],na.rm = T)

sam_table["HH","k"]=sum(sam_table["k",ind_143],na.rm = T)
sam_table["HH","l"]=sum(sam_table["l",ind_143],na.rm = T)

tmp_row_sums = rowSums(sam_table,na.rm = T) # the sum of the columns of each row
tmp_col_sums = colSums(sam_table,na.rm = T) # the sum of the rows of each column
names(tmp_row_sums)=names
names(tmp_col_sums)=names

sam_table$mtt[1:286]=tmp_col_sums[1:286]-tmp_row_sums[1:286]
sam_table["totex","im1"] = tmp_row_sums["im1"]  #sam_table["im1","row_sums"]
sam_table["totex","im2"] = tmp_row_sums["im2"] # sam_table["im2","row_sums"]
sam_table["IS","totex"] = tmp_col_sums["IS"] - sam_table["IS","gov"]

sam_table["gov","income_tax"] = tmp_col_sums["gov"] - tmp_row_sums["gov"]
sam_table["income_tax","HH"] = sam_table["gov","income_tax"] 
sam_table["mtt","cif"] = sum(sam_table$mtt,na.rm=T) - tmp_row_sums["mtt"]

sam_table["HH","cif"] = tmp_row_sums["cif"] - sam_table["mtt","cif"]

sam_table["totex","HH"] = tmp_row_sums["HH"] + sam_table["HH","cif"] - tmp_col_sums["HH"] - sam_table["income_tax","HH"]

#rows and cols sum step 2

row_sums = rowSums(sam_table,na.rm = T)
col_sums = colSums(sam_table,na.rm = T)

sam_table1 = cbind(sam_table,row_sums,col_sums)
sam_table1 = rbind(sam_table1,col_sums)

sam_table1[302,302:303]=c(NA,NA)
colnames(sam_table1)[302]="sum"
rownames(sam_table1)[302]="sum"
sam_table1=cbind(c(names,"sum"),sam_table1)

# saving the 143 industries & commodities SAM
write.xlsx(sam_table1, "mynew.xlsx")


# Choosing the column according to which the compression should be done
compress="FoodModel"
new_ind_names=ind_names[,compress]
unique_new_names=unique(new_ind_names)
lnn=length(unique_new_names)

# define the names of the compressed SAM
new_inds = paste0("ind_",unique_new_names)
new_coms = paste0("com_",unique_new_names)
new_names = c(new_inds , new_coms , rest)

# 1st compression step
med_sam_table <- data.frame(matrix(nrow = length(names), ncol = length(new_names)))
rownames(med_sam_table) <- names
colnames(med_sam_table) <- new_names
dim(med_sam_table)

for (i in 1:lnn) {
    qqq <- new_ind_names==unique_new_names[i]
    med_sam_table[,new_inds[i]]=rowSums(sam_table[,ind_143][,qqq])
    med_sam_table[,new_coms[i]]=rowSums(sam_table[,com_143][,qqq])
}
med_sam_table[,rest]=sam_table[,rest]

# 2nd compression step
new_sam_table <- data.frame(matrix(nrow = length(new_names), ncol = length(new_names)))
rownames(new_sam_table) <- new_names
colnames(new_sam_table) <- new_names
dim(new_sam_table)

for (i in 1:lnn) {
  qqq <- new_ind_names==unique_new_names[i]
  new_sam_table[new_inds[i],]=colSums(med_sam_table[ind_143,][qqq,])
  new_sam_table[new_coms[i],]=colSums(med_sam_table[com_143,][qqq,])
}
new_sam_table[rest,]=med_sam_table[rest,]

# saving the compressed SAM
new_sam_table=cbind(new_names,new_sam_table)
write.xlsx(new_sam_table, "compressed.xlsx")

# creating vector and A-matrix
# Function to create new data frame and divide by column sums
create_df = function(rows, cols, orig_df) {
  # Subset original data frame to rows and columns
  new_df = orig_df[rows, cols]
  # Divide each value in new data frame by column sums in original data frame
  new_df = new_df / (colSums(orig_df[cols],na.rm = T)/2)
  return(new_df)
}

# Example usage of the function
rows <- c("k", "l")
cols <- c("ind1", "ind2")
new_df <- create_df(rows, cols, sam_table1)

# Print new data frame
print(new_df)
colSums(sam_table1["ind1"],na.rm = T)/2









