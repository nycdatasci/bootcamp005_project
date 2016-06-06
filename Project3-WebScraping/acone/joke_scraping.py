#!/usr/bin/python
from bs4 import BeautifulSoup
import requests
import string

text = requests.get('http://slightlywarped.com/incredibly-offensive-jokes/').text
text = BeautifulSoup(text)

joke_tags = text.find_all('li', {'dir': "ltr"})
jokes = []
for joke in joke_tags:
	jokes.append(joke.text.encode('ascii','ignore'))

# create empty curse word dictionary
curse_word_dict = {'shit': 0, 'shits': 0, 'shitted': 0, 'shitting': 0, 'fuck': 0, 'fucks': 0, 'fucking': 0, 'fucked': 0, \
				   'bitch': 0, 'bitches': 0, 'crap': 0, 'craps': 0, 'crapped': 0, 'crapping': 0, 'piss': 0, 'pisses': 0, \
				   'dick': 0, 'dicks': 0, 'cock': 0, 'cocks': 0, 'pussy': 0, 'pussies': 0, 'asshole': 0, 'assholes': 0, \
				   'fag': 0, 'fags': 0, 'faggot': 0, 'faggots': 0, 'slut': 0, 'sluts': 0, 'douche': 0, 'douches': 0, \
				   'cunt': 0, 'cunts': 0, 'fucker': 0, 'dick': 0, 'dicks': 0, 'balls': 0, 'cum': 0, 'retard': 0, \
				   'retarded': 0, 'ass': 0, 'tits': 0}

# get word count, curse count, and exclamation count for each joke in a list
word_count_list = []
curse_count_list = []
exclamation_count_list = []
# loop over all jokes
for joke in jokes:
	# clean out all jokes of punctuation
	joke_nopunct = joke
	for c in string.punctuation:
		joke_nopunct = joke_nopunct.replace(c,'')
	#make lower case and split into word list
	joke_word_list = joke_nopunct.lower().split()
	#get word count
	word_count_list.append(len(joke_word_list))
	# next, get number of curse words
	# initialize empty curse word dictionary
	new_curse_dict = dict(curse_word_dict)
	for word in joke_word_list:
		if word in new_curse_dict.keys():
			new_curse_dict[word] += 1
	curse_count_list.append(sum(new_curse_dict.values()))
	# get number of exclamation points
	exclamation_joke_count = 0
	for char in joke:
		if char == '!':
			exclamation_joke_count += 1
	exclamation_count_list.append(exclamation_joke_count)

worthwhile_list = [''] * len(jokes)
worthwhile_list[1 - 1] = 0
worthwhile_list[2 - 1] = 1
worthwhile_list[3 - 1] = 1
worthwhile_list[4 - 1] = 1
worthwhile_list[5 - 1] = 0
worthwhile_list[6 - 1] = 1
worthwhile_list[7 - 1] = 0
worthwhile_list[8 - 1] = 1
worthwhile_list[9 - 1] = 1
worthwhile_list[10 - 1] = 1
worthwhile_list[11 - 1] = 1
worthwhile_list[12 - 1] = 1
worthwhile_list[13 - 1] = 0
worthwhile_list[14 - 1] = 0
worthwhile_list[15 - 1] = 0
worthwhile_list[16 - 1] = 0
worthwhile_list[17 - 1] = 0
worthwhile_list[18 - 1] = 0
worthwhile_list[19 - 1] = 1
worthwhile_list[20 - 1] = 1
worthwhile_list[21 - 1] = 1
worthwhile_list[22 - 1] = 1
worthwhile_list[23 - 1] = 1
worthwhile_list[24 - 1] = 1
worthwhile_list[25 - 1] = 1
worthwhile_list[26 - 1] = 1
worthwhile_list[27 - 1] = 0
worthwhile_list[28 - 1] = 1
worthwhile_list[29 - 1] = 1
worthwhile_list[30 - 1] = 1
worthwhile_list[31 - 1] = 0
worthwhile_list[32 - 1] = 0
worthwhile_list[33 - 1] = 0
worthwhile_list[34 - 1] = 0
worthwhile_list[35 - 1] = 0
worthwhile_list[36 - 1] = 0
worthwhile_list[37 - 1] = 1
worthwhile_list[38 - 1] = 0
worthwhile_list[39 - 1] = 0
worthwhile_list[40 - 1] = 0
worthwhile_list[41 - 1] = 0
worthwhile_list[42 - 1] = 0
worthwhile_list[43 - 1] = 0
worthwhile_list[44 - 1] = 1
worthwhile_list[45 - 1] = 0
worthwhile_list[46 - 1] = 1
worthwhile_list[47 - 1] = 1
worthwhile_list[48 - 1] = 0
worthwhile_list[49 - 1] = 1
worthwhile_list[50 - 1] = 1
worthwhile_list[51 - 1] = 0
worthwhile_list[52 - 1] = 0
worthwhile_list[53 - 1] = 0
worthwhile_list[54 - 1] = 1
worthwhile_list[55 - 1] = 0
worthwhile_list[56 - 1] = 0
worthwhile_list[57 - 1] = 0
worthwhile_list[58 - 1] = 0
worthwhile_list[59 - 1] = 0
worthwhile_list[60 - 1] = 0
worthwhile_list[61 - 1] = 0
worthwhile_list[62 - 1] = 0
worthwhile_list[63 - 1] = 0
worthwhile_list[64 - 1] = 1
worthwhile_list[65 - 1] = 0
worthwhile_list[66 - 1] = 0
worthwhile_list[67 - 1] = 0
worthwhile_list[68 - 1] = 1
worthwhile_list[69 - 1] = 0
worthwhile_list[70 - 1] = 0
worthwhile_list[71 - 1] = 0
worthwhile_list[72 - 1] = 0
worthwhile_list[73 - 1] = 0
worthwhile_list[74 - 1] = 0
worthwhile_list[75 - 1] = 0
worthwhile_list[76 - 1] = 0
worthwhile_list[77 - 1] = 0
worthwhile_list[78 - 1] = 1
worthwhile_list[79 - 1] = 0
worthwhile_list[80 - 1] = 0
worthwhile_list[81 - 1] = 0
worthwhile_list[82 - 1] = 0
worthwhile_list[83 - 1] = 0
worthwhile_list[84 - 1] = 0
worthwhile_list[85 - 1] = 0
worthwhile_list[86 - 1] = 0
worthwhile_list[87 - 1] = 0
worthwhile_list[88 - 1] = 0
worthwhile_list[89 - 1] = 0
worthwhile_list[90 - 1] = 0
worthwhile_list[91 - 1] = 0
worthwhile_list[92 - 1] = 1
worthwhile_list[93 - 1] = 0
worthwhile_list[94 - 1] = 0
worthwhile_list[95 - 1] = 1
worthwhile_list[96 - 1] = 1
worthwhile_list[97 - 1] = 0
worthwhile_list[98 - 1] = 0
worthwhile_list[99 - 1] = 0
worthwhile_list[100 - 1] = 0
worthwhile_list[101 - 1] = 1
worthwhile_list[102 - 1] = 1
worthwhile_list[103 - 1] = 0
worthwhile_list[104 - 1] = 0
worthwhile_list[105 - 1] = 1
worthwhile_list[106 - 1] = 1
worthwhile_list[107 - 1] = 0
worthwhile_list[108 - 1] = 0
worthwhile_list[109 - 1] = 1
worthwhile_list[110 - 1] = 0
worthwhile_list[111 - 1] = 0
worthwhile_list[112 - 1] = 0
worthwhile_list[113 - 1] = 1
worthwhile_list[114 - 1] = 1
worthwhile_list[115 - 1] = 1
worthwhile_list[116 - 1] = 0
worthwhile_list[117 - 1] = 0
worthwhile_list[118 - 1] = 0
worthwhile_list[119 - 1] = 0
worthwhile_list[120 - 1] = 0
worthwhile_list[121 - 1] = 0
worthwhile_list[122 - 1] = 1
worthwhile_list[123 - 1] = 0
worthwhile_list[124 - 1] = 0
worthwhile_list[125 - 1] = 0
worthwhile_list[126 - 1] = 1
worthwhile_list[127 - 1] = 0
worthwhile_list[128 - 1] = 0
worthwhile_list[129 - 1] = 0
worthwhile_list[130 - 1] = 0
worthwhile_list[131 - 1] = 0
worthwhile_list[132 - 1] = 0
worthwhile_list[133 - 1] = 0
worthwhile_list[134 - 1] = 0
worthwhile_list[135 - 1] = 0
worthwhile_list[136 - 1] = 0
worthwhile_list[137 - 1] = 0
worthwhile_list[138 - 1] = 0
worthwhile_list[139 - 1] = 0
worthwhile_list[140 - 1] = 0
worthwhile_list[141 - 1] = 0
worthwhile_list[142 - 1] = 0
worthwhile_list[143 - 1] = 0
worthwhile_list[144 - 1] = 0
worthwhile_list[145 - 1] = 0
worthwhile_list[146 - 1] = 0
worthwhile_list[147 - 1] = 0
worthwhile_list[148 - 1] = 0
worthwhile_list[149 - 1] = 0
worthwhile_list[150 - 1] = 0
worthwhile_list[151 - 1] = 0
worthwhile_list[152 - 1] = 0
worthwhile_list[153 - 1] = 0
worthwhile_list[154 - 1] = 0
worthwhile_list[155 - 1] = 0
worthwhile_list[156 - 1] = 1
worthwhile_list[157 - 1] = 0
worthwhile_list[158 - 1] = 1
worthwhile_list[159 - 1] = 0
worthwhile_list[160 - 1] = 0
worthwhile_list[161 - 1] = 1
worthwhile_list[162 - 1] = 0
worthwhile_list[163 - 1] = 1
worthwhile_list[164 - 1] = 0
worthwhile_list[165 - 1] = 0
worthwhile_list[166 - 1] = 1
worthwhile_list[167 - 1] = 0
worthwhile_list[168 - 1] = 1
worthwhile_list[169 - 1] = 0
worthwhile_list[170 - 1] = 0
worthwhile_list[171 - 1] = 0
worthwhile_list[172 - 1] = 0
worthwhile_list[173 - 1] = 0
worthwhile_list[174 - 1] = 0
worthwhile_list[175 - 1] = 0
worthwhile_list[176 - 1] = 0
worthwhile_list[177 - 1] = 0
worthwhile_list[178 - 1] = 0
worthwhile_list[179 - 1] = 0
worthwhile_list[180 - 1] = 0
worthwhile_list[181 - 1] = 1
worthwhile_list[182 - 1] = 1
worthwhile_list[183 - 1] = 0
worthwhile_list[184 - 1] = 0
worthwhile_list[185 - 1] = 0
worthwhile_list[186 - 1] = 0
worthwhile_list[187 - 1] = 0
worthwhile_list[188 - 1] = 1
worthwhile_list[189 - 1] = 0
worthwhile_list[190 - 1] = 0
worthwhile_list[191 - 1] = 0
worthwhile_list[192 - 1] = 0
worthwhile_list[193 - 1] = 0
worthwhile_list[194 - 1] = 0
worthwhile_list[195 - 1] = 0
worthwhile_list[196 - 1] = 0
worthwhile_list[197 - 1] = 0
worthwhile_list[198 - 1] = 0
worthwhile_list[199 - 1] = 0
worthwhile_list[200 - 1] = 0
worthwhile_list[201 - 1] = 0
worthwhile_list[202 - 1] = 0
worthwhile_list[203 - 1] = 0
worthwhile_list[204 - 1] = 0
worthwhile_list[205 - 1] = 0
worthwhile_list[206 - 1] = 0
worthwhile_list[207 - 1] = 0
worthwhile_list[208 - 1] = 0
worthwhile_list[209 - 1] = 0
worthwhile_list[210 - 1] = 0
worthwhile_list[211 - 1] = 0
worthwhile_list[212 - 1] = 0
worthwhile_list[213 - 1] = 0
worthwhile_list[214 - 1] = 0
worthwhile_list[215 - 1] = 0
worthwhile_list[216 - 1] = 0
worthwhile_list[217 - 1] = 0
worthwhile_list[218 - 1] = 0
worthwhile_list[219 - 1] = 1
worthwhile_list[220 - 1] = 0
worthwhile_list[221 - 1] = 0
worthwhile_list[222 - 1] = 0
worthwhile_list[223 - 1] = 0
worthwhile_list[224 - 1] = 0
worthwhile_list[225 - 1] = 0
worthwhile_list[226 - 1] = 0
worthwhile_list[227 - 1] = 0
worthwhile_list[228 - 1] = 0
worthwhile_list[229 - 1] = 0
worthwhile_list[230 - 1] = 0
worthwhile_list[231 - 1] = 0
worthwhile_list[232 - 1] = 1
worthwhile_list[233 - 1] = 0
worthwhile_list[234 - 1] = 0
worthwhile_list[235 - 1] = 0
worthwhile_list[236 - 1] = 0
worthwhile_list[237 - 1] = 0
worthwhile_list[238 - 1] = 0
worthwhile_list[239 - 1] = 1
worthwhile_list[240 - 1] = 1
worthwhile_list[241 - 1] = 0
worthwhile_list[242 - 1] = 0
worthwhile_list[243 - 1] = 1
worthwhile_list[244 - 1] = 1
worthwhile_list[245 - 1] = 0
worthwhile_list[246 - 1] = 0
worthwhile_list[247 - 1] = 0
worthwhile_list[248 - 1] = 0
worthwhile_list[249 - 1] = 1
worthwhile_list[250 - 1] = 1
worthwhile_list[251 - 1] = 0
worthwhile_list[252 - 1] = 1
worthwhile_list[253 - 1] = 1
worthwhile_list[254 - 1] = 1
worthwhile_list[255 - 1] = 1
worthwhile_list[256 - 1] = 1
worthwhile_list[257 - 1] = 0
worthwhile_list[258 - 1] = 0
worthwhile_list[259 - 1] = 0
worthwhile_list[260 - 1] = 1
worthwhile_list[261 - 1] = 0
worthwhile_list[262 - 1] = 0
worthwhile_list[263 - 1] = 0
worthwhile_list[264 - 1] = 0
worthwhile_list[265 - 1] = 0
worthwhile_list[266 - 1] = 0
worthwhile_list[267 - 1] = 0
worthwhile_list[268 - 1] = 0
worthwhile_list[269 - 1] = 0
worthwhile_list[270 - 1] = 0
worthwhile_list[271 - 1] = 0
worthwhile_list[272 - 1] = 1
worthwhile_list[273 - 1] = 0
worthwhile_list[274 - 1] = 0
worthwhile_list[275 - 1] = 0
worthwhile_list[276 - 1] = 0
worthwhile_list[277 - 1] = 0
worthwhile_list[278 - 1] = 0
worthwhile_list[279 - 1] = 0
worthwhile_list[280 - 1] = 0
worthwhile_list[281 - 1] = 1
worthwhile_list[282 - 1] = 1
worthwhile_list[283 - 1] = 1
worthwhile_list[284 - 1] = 0
worthwhile_list[285 - 1] = 0
worthwhile_list[286 - 1] = 0
worthwhile_list[287 - 1] = 0
worthwhile_list[288 - 1] = 0
worthwhile_list[289 - 1] = 0
worthwhile_list[290 - 1] = 0
worthwhile_list[291 - 1] = 0
worthwhile_list[292 - 1] = 0
worthwhile_list[293 - 1] = 0
worthwhile_list[294 - 1] = 0
worthwhile_list[295 - 1] = 0
worthwhile_list[296 - 1] = 0
worthwhile_list[297 - 1] = 0
worthwhile_list[298 - 1] = 1
worthwhile_list[299 - 1] = 1
worthwhile_list[300 - 1] = 1
worthwhile_list[301 - 1] = 0
worthwhile_list[302 - 1] = 0
worthwhile_list[303 - 1] = 0
worthwhile_list[304 - 1] = 0
worthwhile_list[305 - 1] = 0
worthwhile_list[306 - 1] = 1
worthwhile_list[307 - 1] = 0
worthwhile_list[308 - 1] = 0
worthwhile_list[309 - 1] = 0
worthwhile_list[310 - 1] = 0
worthwhile_list[311 - 1] = 0
worthwhile_list[312 - 1] = 0
worthwhile_list[313 - 1] = 0
worthwhile_list[314 - 1] = 1
worthwhile_list[315 - 1] = 0
worthwhile_list[316 - 1] = 0
worthwhile_list[317 - 1] = 1
worthwhile_list[318 - 1] = 0
worthwhile_list[319 - 1] = 1
worthwhile_list[320 - 1] = 0
worthwhile_list[321 - 1] = 0
worthwhile_list[322 - 1] = 0
worthwhile_list[323 - 1] = 0
worthwhile_list[324 - 1] = 0
worthwhile_list[325 - 1] = 1
worthwhile_list[326 - 1] = 0
worthwhile_list[327 - 1] = 0
worthwhile_list[328 - 1] = 0
worthwhile_list[329 - 1] = 0
worthwhile_list[330 - 1] = 0
worthwhile_list[331 - 1] = 0
worthwhile_list[332 - 1] = 0
worthwhile_list[333 - 1] = 0
worthwhile_list[334 - 1] = 0
worthwhile_list[335 - 1] = 0
worthwhile_list[336 - 1] = 0
worthwhile_list[337 - 1] = 0
worthwhile_list[338 - 1] = 0
worthwhile_list[339 - 1] = 1
worthwhile_list[340 - 1] = 1
worthwhile_list[341 - 1] = 1
worthwhile_list[342 - 1] = 0
worthwhile_list[343 - 1] = 0
worthwhile_list[344 - 1] = 0
worthwhile_list[345 - 1] = 0
worthwhile_list[346 - 1] = 0
worthwhile_list[347 - 1] = 0
worthwhile_list[348 - 1] = 0
worthwhile_list[349 - 1] = 0
worthwhile_list[350 - 1] = 1
worthwhile_list[351 - 1] = 0
worthwhile_list[352 - 1] = 0
worthwhile_list[353 - 1] = 0
worthwhile_list[354 - 1] = 0
worthwhile_list[355 - 1] = 0
worthwhile_list[356 - 1] = 0
worthwhile_list[357 - 1] = 0
worthwhile_list[358 - 1] = 0
worthwhile_list[359 - 1] = 0
worthwhile_list[360 - 1] = 0
worthwhile_list[361 - 1] = 0
worthwhile_list[362 - 1] = 0
worthwhile_list[363 - 1] = 0
worthwhile_list[364 - 1] = 0
worthwhile_list[365 - 1] = 1
worthwhile_list[366 - 1] = 0
worthwhile_list[367 - 1] = 0
worthwhile_list[368 - 1] = 0
worthwhile_list[369 - 1] = 0
worthwhile_list[370 - 1] = 0
worthwhile_list[371 - 1] = 0
worthwhile_list[372 - 1] = 0
worthwhile_list[373 - 1] = 0
worthwhile_list[374 - 1] = 1
worthwhile_list[375 - 1] = 1
worthwhile_list[376 - 1] = 0
worthwhile_list[377 - 1] = 0
worthwhile_list[378 - 1] = 1
worthwhile_list[379 - 1] = 0
worthwhile_list[380 - 1] = 0
worthwhile_list[381 - 1] = 1
worthwhile_list[382 - 1] = 0
worthwhile_list[383 - 1] = 0
worthwhile_list[384 - 1] = 0
worthwhile_list[385 - 1] = 0
worthwhile_list[386 - 1] = 0
worthwhile_list[387 - 1] = 0

# Next, I want to write the results of my analysis, without the text of the jokes,
# directly into a .txt file.

f = open('joke_data.txt', 'a')
f.write('Joke_Index\tWord_Count\tCurse_Count\tExclamation_Count\tWorthwhile\n')
for i in range(0,len(jokes)):
	f.write('%d\t%d\t%d\t%d\t%d\n' % (i+1,word_count_list[i],curse_count_list[i],\
									  exclamation_count_list[i], worthwhile_list[i]))
f.close()

#for i in range(100,105):
#	print ('%d' + '\t' + '%d' + '\t' + '%d' + '\t' + '%d' + '\t' + '%d') % \
#		(i, word_count_list[i], curse_count_list[i], exclamation_count_list[i], worthwhile_list[i])