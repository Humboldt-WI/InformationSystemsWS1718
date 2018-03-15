#TRANSFORMATION#

# DOWNLOAD PACKAGES
memory.limit(30000)
if(!require("survival")) install.packages("survival"); library("survival")
if(!require("survminer")) install.packages("survminer"); library("survminer")


test <- readRDS("./Survival_analysis/data_2016_rds/all_2016.rds")
#test <- test[,c("date","serial_number","model","smart_9_normalized","smart_5_normalized","smart_187_normalized"
#                ,"smart_188_normalized","smart_197_normalized","smart_198_normalized","failure")]
test <- test[,c("date","serial_number","model","smart_9_raw","smart_5_raw","smart_187_raw"
                ,"smart_188_raw","smart_197_raw","smart_198_raw","failure")]
test$date <- as.character(test$date )
test[test$date == "2016-01-01",]$date <- 1
test[test$date == "2016-01-02",]$date <- 2
test[test$date == "2016-01-03",]$date <- 3
test[test$date == "2016-01-04",]$date <- 4
test[test$date == "2016-01-05",]$date <- 5
test[test$date == "2016-01-06",]$date <- 6
test[test$date == "2016-01-07",]$date <- 7
test[test$date == "2016-01-08",]$date <- 8
test[test$date == "2016-01-09",]$date <- 9
test[test$date == "2016-01-10",]$date <- 10
test[test$date == "2016-01-11",]$date <- 11
test[test$date == "2016-01-12",]$date <- 12
test[test$date == "2016-01-13",]$date <- 13
test[test$date == "2016-01-14",]$date <- 14
test[test$date == "2016-01-15",]$date <- 15
test[test$date == "2016-01-16",]$date <- 16
test[test$date == "2016-01-17",]$date <- 17
test[test$date == "2016-01-18",]$date <- 18
test[test$date == "2016-01-19",]$date <- 19
test[test$date == "2016-01-20",]$date <- 20
test[test$date == "2016-01-21",]$date <- 21
test[test$date == "2016-01-22",]$date <- 22
test[test$date == "2016-01-23",]$date <- 23
test[test$date == "2016-01-24",]$date <- 24
test[test$date == "2016-01-25",]$date <- 25
test[test$date == "2016-01-26",]$date <- 26
test[test$date == "2016-01-27",]$date <- 27
test[test$date == "2016-01-28",]$date <- 28
test[test$date == "2016-01-29",]$date <- 29
test[test$date == "2016-01-30",]$date <- 30
test[test$date == "2016-01-31",]$date <- 31
test[test$date == "2016-02-01",]$date <- 32
test[test$date == "2016-02-02",]$date <- 33
test[test$date == "2016-02-03",]$date <- 34
test[test$date == "2016-02-04",]$date <- 35
test[test$date == "2016-02-05",]$date <- 36
test[test$date == "2016-02-06",]$date <- 37
test[test$date == "2016-02-07",]$date <- 38
test[test$date == "2016-02-08",]$date <- 39
test[test$date == "2016-02-09",]$date <- 40
test[test$date == "2016-02-10",]$date <- 41
test[test$date == "2016-02-11",]$date <- 42
test[test$date == "2016-02-12",]$date <- 43
test[test$date == "2016-02-13",]$date <- 44
test[test$date == "2016-02-14",]$date <- 45
test[test$date == "2016-02-15",]$date <- 46
test[test$date == "2016-02-16",]$date <- 47
test[test$date == "2016-02-17",]$date <- 48
test[test$date == "2016-02-18",]$date <- 49
test[test$date == "2016-02-19",]$date <- 50
test[test$date == "2016-02-20",]$date <- 51
test[test$date == "2016-02-21",]$date <- 52
test[test$date == "2016-02-22",]$date <- 53
test[test$date == "2016-02-23",]$date <- 54
test[test$date == "2016-02-24",]$date <- 55
test[test$date == "2016-02-25",]$date <- 56
test[test$date == "2016-02-26",]$date <- 57
test[test$date == "2016-02-27",]$date <- 58
test[test$date == "2016-02-28",]$date <- 59
test[test$date == "2016-02-29",]$date <- 60
test[test$date == "2016-03-01",]$date <- 61
test[test$date == "2016-03-02",]$date <- 62
test[test$date == "2016-03-03",]$date <- 63
test[test$date == "2016-03-04",]$date <- 64
test[test$date == "2016-03-05",]$date <- 65
test[test$date == "2016-03-06",]$date <- 66
test[test$date == "2016-03-07",]$date <- 67
test[test$date == "2016-03-08",]$date <- 68
test[test$date == "2016-03-09",]$date <- 69
test[test$date == "2016-03-10",]$date <- 70
test[test$date == "2016-03-11",]$date <- 71
test[test$date == "2016-03-12",]$date <- 72
test[test$date == "2016-03-13",]$date <- 73
test[test$date == "2016-03-14",]$date <- 74
test[test$date == "2016-03-15",]$date <- 75
test[test$date == "2016-03-16",]$date <- 76
test[test$date == "2016-03-17",]$date <- 77
test[test$date == "2016-03-18",]$date <- 78
test[test$date == "2016-03-19",]$date <- 79
test[test$date == "2016-03-20",]$date <- 80
test[test$date == "2016-03-21",]$date <- 81
test[test$date == "2016-03-22",]$date <- 82
test[test$date == "2016-03-23",]$date <- 83
test[test$date == "2016-03-24",]$date <- 84
test[test$date == "2016-03-25",]$date <- 85
test[test$date == "2016-03-26",]$date <- 86
test[test$date == "2016-03-27",]$date <- 87
test[test$date == "2016-03-28",]$date <- 88
test[test$date == "2016-03-29",]$date <- 89
test[test$date == "2016-03-30",]$date <- 90
test[test$date == "2016-03-31",]$date <- 91
test[test$date == "2016-04-01",]$date <- 92
test[test$date == "2016-04-02",]$date <- 93
test[test$date == "2016-04-03",]$date <- 94
test[test$date == "2016-04-04",]$date <- 95
test[test$date == "2016-04-05",]$date <- 96
test[test$date == "2016-04-06",]$date <- 97
test[test$date == "2016-04-07",]$date <- 98
test[test$date == "2016-04-08",]$date <- 99
test[test$date == "2016-04-09",]$date <- 100
test[test$date == "2016-04-10",]$date <- 101
test[test$date == "2016-04-11",]$date <- 102
test[test$date == "2016-04-12",]$date <- 103
test[test$date == "2016-04-13",]$date <- 104
test[test$date == "2016-04-14",]$date <- 105
test[test$date == "2016-04-15",]$date <- 106
test[test$date == "2016-04-16",]$date <- 107
test[test$date == "2016-04-17",]$date <- 108
test[test$date == "2016-04-18",]$date <- 109
test[test$date == "2016-04-19",]$date <- 110
test[test$date == "2016-04-20",]$date <- 111
test[test$date == "2016-04-21",]$date <- 112
test[test$date == "2016-04-22",]$date <- 113
test[test$date == "2016-04-23",]$date <- 114
test[test$date == "2016-04-24",]$date <- 115
test[test$date == "2016-04-25",]$date <- 116
test[test$date == "2016-04-26",]$date <- 117
test[test$date == "2016-04-27",]$date <- 118
test[test$date == "2016-04-28",]$date <- 119
test[test$date == "2016-04-29",]$date <- 120
test[test$date == "2016-04-30",]$date <- 121
test[test$date == "2016-05-01",]$date <- 122
test[test$date == "2016-05-02",]$date <- 123
test[test$date == "2016-05-03",]$date <- 124
test[test$date == "2016-05-04",]$date <- 125
test[test$date == "2016-05-05",]$date <- 126
test[test$date == "2016-05-06",]$date <- 127
test[test$date == "2016-05-07",]$date <- 128
test[test$date == "2016-05-08",]$date <- 129
test[test$date == "2016-05-09",]$date <- 130
test[test$date == "2016-05-10",]$date <- 131
test[test$date == "2016-05-11",]$date <- 132
test[test$date == "2016-05-12",]$date <- 133
test[test$date == "2016-05-13",]$date <- 134
test[test$date == "2016-05-14",]$date <- 135
test[test$date == "2016-05-15",]$date <- 136
test[test$date == "2016-05-16",]$date <- 137
test[test$date == "2016-05-17",]$date <- 138
test[test$date == "2016-05-18",]$date <- 139
test[test$date == "2016-05-19",]$date <- 140
test[test$date == "2016-05-20",]$date <- 141
test[test$date == "2016-05-21",]$date <- 142
test[test$date == "2016-05-22",]$date <- 143
test[test$date == "2016-05-23",]$date <- 144
test[test$date == "2016-05-24",]$date <- 145
test[test$date == "2016-05-25",]$date <- 146
test[test$date == "2016-05-26",]$date <- 147
test[test$date == "2016-05-27",]$date <- 148
test[test$date == "2016-05-28",]$date <- 149
test[test$date == "2016-05-29",]$date <- 150
test[test$date == "2016-05-30",]$date <- 151
test[test$date == "2016-05-31",]$date <- 152
test[test$date == "2016-06-01",]$date <- 153
test[test$date == "2016-06-02",]$date <- 154
test[test$date == "2016-06-03",]$date <- 155
test[test$date == "2016-06-04",]$date <- 156
test[test$date == "2016-06-05",]$date <- 157
test[test$date == "2016-06-06",]$date <- 158
test[test$date == "2016-06-07",]$date <- 159
test[test$date == "2016-06-08",]$date <- 160
test[test$date == "2016-06-09",]$date <- 161
test[test$date == "2016-06-10",]$date <- 162
test[test$date == "2016-06-11",]$date <- 163
test[test$date == "2016-06-12",]$date <- 164
test[test$date == "2016-06-13",]$date <- 165
test[test$date == "2016-06-14",]$date <- 166
test[test$date == "2016-06-15",]$date <- 167
test[test$date == "2016-06-16",]$date <- 168
test[test$date == "2016-06-17",]$date <- 169
test[test$date == "2016-06-18",]$date <- 170
test[test$date == "2016-06-19",]$date <- 171
test[test$date == "2016-06-20",]$date <- 172
test[test$date == "2016-06-21",]$date <- 173
test[test$date == "2016-06-22",]$date <- 174
test[test$date == "2016-06-23",]$date <- 175
test[test$date == "2016-06-24",]$date <- 176
test[test$date == "2016-06-25",]$date <- 177
test[test$date == "2016-06-26",]$date <- 178
test[test$date == "2016-06-27",]$date <- 179
test[test$date == "2016-06-28",]$date <- 180
test[test$date == "2016-06-29",]$date <- 181
test[test$date == "2016-06-30",]$date <- 182
test[test$date == "2016-07-01",]$date <- 183
test[test$date == "2016-07-02",]$date <- 184
test[test$date == "2016-07-03",]$date <- 185
test[test$date == "2016-07-04",]$date <- 186
test[test$date == "2016-07-05",]$date <- 187
test[test$date == "2016-07-06",]$date <- 188
test[test$date == "2016-07-07",]$date <- 189
test[test$date == "2016-07-08",]$date <- 190
test[test$date == "2016-07-09",]$date <- 191
test[test$date == "2016-07-10",]$date <- 192
test[test$date == "2016-07-11",]$date <- 193
test[test$date == "2016-07-12",]$date <- 194
test[test$date == "2016-07-13",]$date <- 195
test[test$date == "2016-07-14",]$date <- 196
test[test$date == "2016-07-15",]$date <- 197
test[test$date == "2016-07-16",]$date <- 198
test[test$date == "2016-07-17",]$date <- 199
test[test$date == "2016-07-18",]$date <- 200
test[test$date == "2016-07-19",]$date <- 201
test[test$date == "2016-07-20",]$date <- 202
test[test$date == "2016-07-21",]$date <- 203
test[test$date == "2016-07-22",]$date <- 204
test[test$date == "2016-07-23",]$date <- 205
test[test$date == "2016-07-24",]$date <- 206
test[test$date == "2016-07-25",]$date <- 207
test[test$date == "2016-07-26",]$date <- 208
test[test$date == "2016-07-27",]$date <- 209
test[test$date == "2016-07-28",]$date <- 210
test[test$date == "2016-07-29",]$date <- 211
test[test$date == "2016-07-30",]$date <- 212
test[test$date == "2016-07-31",]$date <- 213
test[test$date == "2016-08-01",]$date <- 214
test[test$date == "2016-08-02",]$date <- 215
test[test$date == "2016-08-03",]$date <- 216
test[test$date == "2016-08-04",]$date <- 217
test[test$date == "2016-08-05",]$date <- 218
test[test$date == "2016-08-06",]$date <- 219
test[test$date == "2016-08-07",]$date <- 220
test[test$date == "2016-08-08",]$date <- 221
test[test$date == "2016-08-09",]$date <- 222
test[test$date == "2016-08-10",]$date <- 223
test[test$date == "2016-08-11",]$date <- 224
test[test$date == "2016-08-12",]$date <- 225
test[test$date == "2016-08-13",]$date <- 226
test[test$date == "2016-08-14",]$date <- 227
test[test$date == "2016-08-15",]$date <- 228
test[test$date == "2016-08-16",]$date <- 229
test[test$date == "2016-08-17",]$date <- 230
test[test$date == "2016-08-18",]$date <- 231
test[test$date == "2016-08-19",]$date <- 232
test[test$date == "2016-08-20",]$date <- 233
test[test$date == "2016-08-21",]$date <- 234
test[test$date == "2016-08-22",]$date <- 235
test[test$date == "2016-08-23",]$date <- 236
test[test$date == "2016-08-24",]$date <- 237
test[test$date == "2016-08-25",]$date <- 238
test[test$date == "2016-08-26",]$date <- 239
test[test$date == "2016-08-27",]$date <- 240
test[test$date == "2016-08-28",]$date <- 241
test[test$date == "2016-08-29",]$date <- 242
test[test$date == "2016-08-30",]$date <- 243
test[test$date == "2016-08-31",]$date <- 244
test[test$date == "2016-09-01",]$date <- 245
test[test$date == "2016-09-02",]$date <- 246
test[test$date == "2016-09-03",]$date <- 247
test[test$date == "2016-09-04",]$date <- 248
test[test$date == "2016-09-05",]$date <- 249
test[test$date == "2016-09-06",]$date <- 250
test[test$date == "2016-09-07",]$date <- 251
test[test$date == "2016-09-08",]$date <- 252
test[test$date == "2016-09-09",]$date <- 253
test[test$date == "2016-09-10",]$date <- 254
test[test$date == "2016-09-11",]$date <- 255
test[test$date == "2016-09-12",]$date <- 256
test[test$date == "2016-09-13",]$date <- 257
test[test$date == "2016-09-14",]$date <- 258
test[test$date == "2016-09-15",]$date <- 259
test[test$date == "2016-09-16",]$date <- 260
test[test$date == "2016-09-17",]$date <- 261
test[test$date == "2016-09-18",]$date <- 262
test[test$date == "2016-09-19",]$date <- 263
test[test$date == "2016-09-20",]$date <- 264
test[test$date == "2016-09-21",]$date <- 265
test[test$date == "2016-09-22",]$date <- 266
test[test$date == "2016-09-23",]$date <- 267
test[test$date == "2016-09-24",]$date <- 268
test[test$date == "2016-09-25",]$date <- 269
test[test$date == "2016-09-26",]$date <- 270
test[test$date == "2016-09-27",]$date <- 271
test[test$date == "2016-09-28",]$date <- 272
test[test$date == "2016-09-29",]$date <- 273
test[test$date == "2016-09-30",]$date <- 274
test[test$date == "2016-10-01",]$date <- 275
test[test$date == "2016-10-02",]$date <- 276
test[test$date == "2016-10-03",]$date <- 277
test[test$date == "2016-10-04",]$date <- 278
test[test$date == "2016-10-05",]$date <- 279
test[test$date == "2016-10-06",]$date <- 280
test[test$date == "2016-10-07",]$date <- 281
test[test$date == "2016-10-08",]$date <- 282
test[test$date == "2016-10-09",]$date <- 283
test[test$date == "2016-10-10",]$date <- 284
test[test$date == "2016-10-11",]$date <- 285
test[test$date == "2016-10-12",]$date <- 286
test[test$date == "2016-10-13",]$date <- 287
test[test$date == "2016-10-14",]$date <- 288
test[test$date == "2016-10-15",]$date <- 289
test[test$date == "2016-10-16",]$date <- 290
test[test$date == "2016-10-17",]$date <- 291
test[test$date == "2016-10-18",]$date <- 292
test[test$date == "2016-10-19",]$date <- 293
test[test$date == "2016-10-20",]$date <- 294
test[test$date == "2016-10-21",]$date <- 295
test[test$date == "2016-10-22",]$date <- 296
test[test$date == "2016-10-23",]$date <- 297
test[test$date == "2016-10-24",]$date <- 298
test[test$date == "2016-10-25",]$date <- 299
test[test$date == "2016-10-26",]$date <- 300
test[test$date == "2016-10-27",]$date <- 301
test[test$date == "2016-10-28",]$date <- 302
test[test$date == "2016-10-29",]$date <- 303
test[test$date == "2016-10-30",]$date <- 304
test[test$date == "2016-10-31",]$date <- 305
test[test$date == "2016-11-01",]$date <- 306
test[test$date == "2016-11-02",]$date <- 307
test[test$date == "2016-11-03",]$date <- 308
test[test$date == "2016-11-04",]$date <- 309
test[test$date == "2016-11-05",]$date <- 310
test[test$date == "2016-11-06",]$date <- 311
test[test$date == "2016-11-07",]$date <- 312
test[test$date == "2016-11-08",]$date <- 313
test[test$date == "2016-11-09",]$date <- 314
test[test$date == "2016-11-10",]$date <- 315
test[test$date == "2016-11-11",]$date <- 316
test[test$date == "2016-11-12",]$date <- 317
test[test$date == "2016-11-13",]$date <- 318
test[test$date == "2016-11-14",]$date <- 319
test[test$date == "2016-11-15",]$date <- 320
test[test$date == "2016-11-16",]$date <- 321
test[test$date == "2016-11-17",]$date <- 322
test[test$date == "2016-11-18",]$date <- 323
test[test$date == "2016-11-19",]$date <- 324
test[test$date == "2016-11-20",]$date <- 325
test[test$date == "2016-11-21",]$date <- 326
test[test$date == "2016-11-22",]$date <- 327
test[test$date == "2016-11-23",]$date <- 328
test[test$date == "2016-11-24",]$date <- 329
test[test$date == "2016-11-25",]$date <- 330
test[test$date == "2016-11-26",]$date <- 331
test[test$date == "2016-11-27",]$date <- 332
test[test$date == "2016-11-28",]$date <- 333
test[test$date == "2016-11-29",]$date <- 334
test[test$date == "2016-11-30",]$date <- 335
test[test$date == "2016-12-01",]$date <- 336
test[test$date == "2016-12-02",]$date <- 337
test[test$date == "2016-12-03",]$date <- 338
test[test$date == "2016-12-04",]$date <- 339
test[test$date == "2016-12-05",]$date <- 340
test[test$date == "2016-12-06",]$date <- 341
test[test$date == "2016-12-07",]$date <- 342
test[test$date == "2016-12-08",]$date <- 343
test[test$date == "2016-12-09",]$date <- 344
test[test$date == "2016-12-10",]$date <- 345
test[test$date == "2016-12-11",]$date <- 346
test[test$date == "2016-12-12",]$date <- 347
test[test$date == "2016-12-13",]$date <- 348
test[test$date == "2016-12-14",]$date <- 349
test[test$date == "2016-12-15",]$date <- 350
test[test$date == "2016-12-16",]$date <- 351
test[test$date == "2016-12-17",]$date <- 352
test[test$date == "2016-12-18",]$date <- 353
test[test$date == "2016-12-19",]$date <- 354
test[test$date == "2016-12-20",]$date <- 355
test[test$date == "2016-12-21",]$date <- 356
test[test$date == "2016-12-22",]$date <- 357
test[test$date == "2016-12-23",]$date <- 358
test[test$date == "2016-12-24",]$date <- 359
test[test$date == "2016-12-25",]$date <- 360
test[test$date == "2016-12-26",]$date <- 361
test[test$date == "2016-12-27",]$date <- 362
test[test$date == "2016-12-28",]$date <- 363
test[test$date == "2016-12-29",]$date <- 364
test[test$date == "2016-12-30",]$date <- 365
test[test$date == "2016-12-31",]$date <- 366
test$date <- as.numeric(test$date)

#saveRDS(test, paste0("./Survival_analysis/data_2016_rds/","prep_set_raw",".rds")) #raw




a <- rbind(test[test$date == 1, ],test[test$date == 90, ])
a <- rbind(a,test[test$date == 180, ])
a <- rbind(a,test[test$date == 270, ])
a <- rbind(a,test[test$date == 366, ])
a <- a[!a$failure == 1, ] 
a <- rbind(a,test[test$failure == 1, ])




a[a$failure == 1 & a$date <= 90 ,]$date <- 90
a[a$failure == 1 & a$date >90 & a$date <= 180 ,]$date <- 180
a[a$failure == 1 & a$date >180 & a$date <= 270 ,]$date <- 270
a[a$failure == 1 & a$date >270 & a$date <= 366 ,]$date <- 366
a$serial_number <- as.character(a$serial_number)

#saveRDS(a, paste0("./Survival_analysis/data_2016_rds/","prep_set_raw_small",".rds")) #raw

###MODELLING###
a <- readRDS("./Survival_analysis/data_2016_rds/prep_set_raw_small.rds")

c <- a[!is.na(a$smart_5_raw) & !is.na(a$smart_187_raw) & !is.na(a$smart_188_raw) & !is.na(a$smart_197_raw) & !is.na(a$smart_198_raw),]


c$startdate <- 0
c[c$date ==1,]$startdate <- 0
c[c$date ==90,]$startdate <- 1
c[c$date ==180,]$startdate <-90
c[c$date ==270,]$startdate <- 180
c[c$date ==366,]$startdate <- 270


trainsn <- unique(c$serial_number)
trainsn <- trainsn[sample(NROW(trainsn))]
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = NROW(trainsn), size = floor(.75*NROW(trainsn)), replace = F)
train <- trainsn[sample ]
test  <- trainsn[-sample ]

trainset <- c[c$serial_number %in% train,]
testset <- c[c$serial_number %in% test,]



#c_short <- c[c$smart_9_raw < summary(c$smart_9_raw)[2]]
#c_med <- c[c$smart_9_raw >= summary(c$smart_9_raw)[2] & c$smart_9_raw < summary(c$smart_9_raw)[4]]
#c_long <- c[c$smart_9_raw >= summary(c$smart_9_raw)[4] & c$smart_9_raw <  summary(c$smart_9_raw)[5]]
#c_verylong <- c[c$smart_9_raw >= summary(c$smart_9_raw)[5]]

## run cox model
#c_short.cox <- coxph(Surv(startdate,date, failure) ~ smart_9_raw +smart_5_raw +smart_187_raw +smart_188_raw+smart_197_raw,
#              data = c)
#c_med.cox <- coxph(Surv(startdate,date, failure) ~ smart_5_raw +smart_187_raw +smart_188_raw+smart_197_raw,
#                    data = c_med)
#c_long.cox <- coxph(Surv(startdate,date, failure) ~ smart_5_raw +smart_187_raw +smart_188_raw+smart_197_raw,
#                    data = c_long)
#c_verylong.cox <- coxph(Surv(startdate,date, failure) ~ smart_5_raw +smart_187_raw +smart_188_raw+smart_197_raw,
#                   data = c_verylong)
train.cox <- coxph(Surv(startdate,date, failure) ~ smart_9_raw +smart_5_raw +smart_187_raw +smart_188_raw+smart_197_raw,
                   data = trainset)
(c$serial_number)

#objects for model results, this is where you throw new values at an existing model
shortfit <- survfit(c_short.cox)
medfit <- survfit(c_med.cox)
longfit <- survfit(c_long.cox)
verylongfit <- survfit(c_verylong.cox)

new <- testset[testset$serial_number == "Z3029Z3P",]

trainfit <- survfit(train.cox,new)
trainfit$surv

#prepare graph and print

q1 <- rep("Q1", length(shortfit$time))
q1df <- data.frame(shortfit$time,shortfit$surv,q1)
names(q1df) <- c("Time","Surv","Category")

q2 <- rep("Q2", length(medfit$time))
q2df <- data.frame(medfit$time,medfit$surv,q2)
names(q2df) <- c("Time","Surv","Category")

q3 <- rep("Q3", length(longfit$time))
q3df <- data.frame(longfit$time,longfit$surv,q3)
names(q3df) <- c("Time","Surv","Category")

q4 <- rep("Q4", length(verylongfit$time))
q4df <- data.frame(verylongfit$time,verylongfit$surv,q4)
names(q4df) <- c("Time","Surv","Category")

qplot <- rbind(q1df,q2df,q3df,q4df)


p <- ggplot(qplot, aes(x = Time, y = Surv, color = Category))
p + geom_line() + ggtitle("Comparison of Survival Curves") 

#cutdates <- unique(c$date[c$failure == 1])

#SURV <- survSplit(data = c, cut = cutdates, end = "date",start = "tstart", event = "failure")
#SURV <- unique(SURV)



#plot(basehaz(c_short.cox))
#plot(basehaz(c_verylong.cox))






