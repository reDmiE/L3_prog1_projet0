# Input the data file into a named table
data_times <- read.table("times.data", sep=",", col.names=c("Siscs","Steps"))

# Specify the output file and format and open it
pdf("times.pdf")

# Graph the table using blue points overlayed by a line
plot(data_times, type="o", col="blue")

# Create a title with a red, bold/italic font
title(main="Number of steps needed", col.main="red", font.main=4)

# Close the output device
dev.off()

# quit the system
q()
