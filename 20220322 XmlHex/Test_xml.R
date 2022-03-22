library(xml2)
read_xml("xml_hex_problem.xml")
# 当然可以通过try()获取其运行结果，再判断其是否是“try-error”，如果是就可以跳过
res_error <- try(read_xml("xml_hex_problem.xml"))
class(res_error) == "try-error"

