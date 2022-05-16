library(stargazer)
library(broom)

# descriptive statistics
stargazer(latin_base, title = "描述性统计分析", type = "html",
          no.space = TRUE, summary.stat = c('median','mean','sd','min','max'),
          out = "/Users/yangjingxi/Desktop/国关毕设/IR_Dissertation/123.html")

# DID RESULT
did_summary <- tidy(example_attgt)
stargazer(did_summary, title = "多期倍分法分组结果", type = "html",
          no.space = TRUE, digits= 3,
          out = "/Users/yangjingxi/Desktop/国关毕设/IR_Dissertation/DID_group.html")

# FIX RESULT
stargazer(fix_within_bri, fix_within_cum, fix_within_val, 
          title = "固定效应模型", type = "html",
          no.space = TRUE, digits= 3,
          dep.var.labels=c("BRI","项目数量","价值"),
          covariate.labels=c("BRI协议", "项目数量","营业额(对数)",
                             "人均GDP(对数)","人口(对数)","对中国进口依赖度",
                             "年龄","性别","职业","教育水平","社会经济地位","当地经济情况",
                             "个人意识形态","与中国外交立场距离","政体","BRI协议×社会经济地位",
                             "BRI协议×教育水平","项目数量×社会经济地位","项目数量×教育水平",
                             "营业额(对数)×社会经济地位","营业额(对数)×教育水平"),
          out = "/Users/yangjingxi/Desktop/国关毕设/IR_Dissertation/fix.html")

## random result
stargazer(fix_random_bri, fix_random_cum, fix_random_val, 
          title = "随机效应模型", type = "html",
          no.space = TRUE, digits= 3,
          dep.var.labels=c("协议","项目数量","价值"),
          covariate.labels=c("BRI协议", "项目数量","营业额(对数)",
                             "人均GDP(对数)","人口(对数)","对中国进口依赖度",
                             "年龄","性别","职业","教育水平","社会经济地位","当地经济情况",
                             "个人意识形态","与中国外交立场距离","政体","BRI协议×社会经济地位",
                             "BRI协议×教育水平","项目数量×社会经济地位","项目数量×教育水平",
                             "营业额(对数)×社会经济地位","营业额(对数)×教育水平"),
          out = "/Users/yangjingxi/Desktop/国关毕设/IR_Dissertation/random.html")

## logit
logit_fix_bri <- tidy(logit_fix_bri)
stargazer(logit_fix_bri, logit_fix_cum, logit_fix_val, 
          title = "Logit固定效应模型", type = "html",
          no.space = TRUE, digits= 3,
          dep.var.labels=c("协议","项目数量","价值"),
          covariate.labels=c("BRI协议", "项目数量","营业额(对数)",
                             "人均GDP(对数)","人口(对数)","对中国进口依赖度",
                             "年龄","性别","职业","教育水平","社会经济地位","当地经济情况",
                             "个人意识形态","与中国外交立场距离","政体","BRI协议×社会经济地位",
                             "BRI协议×教育水平","项目数量×社会经济地位","项目数量×教育水平",
                             "营业额(对数)×社会经济地位","营业额(对数)×教育水平"),
          out = "/Users/yangjingxi/Desktop/国关毕设/IR_Dissertation/random.html")

