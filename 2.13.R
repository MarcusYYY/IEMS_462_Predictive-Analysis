data_steak <- read.csv("Steak Prices-1.CSV")
chuck_qty <- comReturn(data_steak$Chuck.Qty,1,47)

chuck_price <- as.numeric(sub("\\$","", data_steak$Chuck.Price))
chuck_price <- com_Return(chuck_price,1,47)

port_price <- as.numeric(sub("\\$","", data_steak$PortHse.Price))
port_price <- com_Return(port_price,1,47)

rib_price <- as.numeric(sub("\\$","", data_steak$RibEye.Price))
rib_price <- com_Return(rib_price,1,47)

port_qty <- com_Return(data_steak$PortHse.Qty,1,47)
rib_qty <- com_Return(data_steak$RibEye.Qty,1,47)

fit_chuck=lm(chuck_qty~chuck_price)
summary(fit_chuck)

fit_rib =lm(rib_qty~rib_price)
summary(fit_rib)

fit_port <- lm(port_qty~port_price)
summary(fit_port)