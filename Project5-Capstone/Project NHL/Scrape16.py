import pandas as pd
from urllib2 import urlopen
import json
import requests
from lxml import html
import requests
import codecs

def get_json_data(url):
    try:
        response = urlopen(url)
        data = str(response.read())
        return json.loads(data)
    except:
        return None

def get_Schedule(Yr, Mo, TeamID):
    schedule = pd.DataFrame(columns=schedCols)
    try:
        hLink = 'http://nhlwc.cdnak.neulion.com/fs1/nhl/league/clubschedule/' + TeamID + '/' \
                + str(Yr) + '/' + str(Mo) + '/iphone/clubschedule.json'
        data = get_json_data(hLink)
        df = pd.DataFrame(data['games'])
        df['Team'] = TeamID
        df['Yr'] = Yr
        df['Mo'] = Mo
        schedule = pd.concat([schedule, df], axis=0)
    except:
        pass
    return schedule

def parseGameCode(CD):
    S = str(int(CD))
    return [int(S[0:4]), int(S[4:6]), int(S[-4:])]

def getURL(seas, gametype, gamenum, repcode):
    url = ['http://www.nhl.com/scores/htmlreports/', str(seas - 1), str(seas),
           '/', repcode, '0', str(gametype), ('%04i' % (gamenum)), '.HTM']
    return ''.join(url)

def getReport(gameCode, ReportCode):
    GC = parseGameCode(gameCode)
    url = getURL(GC[0], GC[1], GC[2], ReportCode)
    req = requests.get(url, headers={
        'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11',
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
        'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
        'Accept-Encoding': 'none',
        'Accept-Language': 'en-US,en;q=0.8',
        'Connection': 'keep-alive'
    })
    return req

def SaveEvt(f, gameId, per, playNo, event, desc, time, stg):
    T = str(int(gameId)) + ',' + str(per) + ',' + str(
        playNo) + ',\"' + event + '\",\"' + desc + '\",' + time + ',' + stg
    file = codecs.open(f, 'a', 'utf-8')
    file.write(T + '\n')
    file.close()

def SaveEvtRO(f, gameId, per, playNo, ha, plNo, plNa, PlPo):
    T = str(int(gameId)) + ',' + str(per) + ',' + str(playNo) + ',' + ha + ',' + plNo + ',' + plNa + ',' + PlPo
    file = codecs.open(f, 'a', 'utf-8')
    file.write(T + '\n')
    file.close()


if __name__ == '__main__':
    NHLTeams = ['ANA', 'ARI', 'BOS', 'BUF', 'CAR', 'CBJ', 'CGY', 'CHI', 'COL', 'DAL', 'DET', 'EDM', \
                'FLA', 'LAK', 'MIN', 'MTL', 'NJD', 'NSH', 'NYI', 'NYR', 'OTT', 'PHI', 'PHX', 'PIT', \
                'SJS', 'STL', 'TBL', 'TOR', 'VAN', 'WPG', 'WSH']
    rosterColumns = ['Team', 'birthplace', 'name', 'weight', 'imageUrl', 'number', 'birthdate', \
                    'height', 'age', 'position', 'id', 'twitterHandle', 'twitterURL']
    schedCols = ['Team', 'Yr', 'Mo', 'abb', 'aud', 'cPeriod', 'gameId', 'gs', 'hg', 'loc', 'score', 'startTime',
                'status', 'usTvLocalMsg', 'vid']

    shifts = pd.DataFrame(columns=['GameId', 'ha', 'PNum', 'durMin', 'durSec', 'Event', 'Pd', 'ShiftNo', 'strtMin', 'strtSec'])

    Seasons = [2016]
    Months = range(1,13)
    Schedule = pd.DataFrame(columns=schedCols)
    for TeamID in NHLTeams:
        for Yr in Seasons:
            for Mo in Months:
                df = get_Schedule(Yr, Mo, TeamID)
                Schedule = pd.concat([Schedule, df], axis=0)

    ColMap = {'play_num': 0, 'per': 1, 'str': 2, 'time': 3, 'event': 4, 'desc': 5, 'vis': 6, 'home': 7}
    EventFName = 'events.csv'
    EvtROFName = 'eventRO.csv'

    # Load existing data
    try:
        ExistingRecs = pd.read_csv(EventFName)
        ExistingGames = map(int, ExistingRecs.gameId)
    except:
        ExistingGames = []
        Head1 = 'gameId,per,playNo,event,desc,time,stg'
        file = codecs.open(EventFName, "w", "utf-8")
        file.write(Head1 + '\n')
        file.close()

        Head2 = 'gameId, per, playNo, ha, plNo, plNa, PlPo'
        file = codecs.open(EvtROFName, "w", "utf-8")
        file.write(Head2 + '\n')
        file.close()

    for N, G in Schedule.iterrows():
        if G.gameId not in ExistingGames:
            HTML_Obj = getReport(G.gameId, 'PL')
            hTree = html.fromstring(HTML_Obj.content)
            plays = hTree.xpath('//tr[@class = "evenColor"]')
            for P in plays:
                P2 = P.findall('./td')
                playNo = P2[ColMap['play_num']].text
                per = P2[ColMap['per']].text
                stre = P2[ColMap['str']].text
                evt = P2[ColMap['event']].text
                desc = P2[ColMap['desc']].text
                time = P2[ColMap['time']].text
                SaveEvt(EventFName, G.gameId, per, playNo, evt, desc, time, stre)
                try:
                    vis = P2[ColMap['vis']].xpath('./table')[0][0]
                    for td in vis.iterchildren():
                        pl_data = td.xpath('./table/tr')
                        if len(pl_data):
                            pl = pl_data[0].xpath('./td/font')
                            if pl[0].text.isdigit():
                                plNum = pl[0].text
                                plName = pl[0].get('title').split('-')[1]
                                plPos = pl[0].get('title').split('-')[0]
                                SaveEvtRO(EvtROFName, G.gameId, per, playNo, 'a', plNum, plName, plPos)
                except:
                    pass

                try:
                    home = P2[ColMap['home']].xpath('./table')[0][0]
                    for td in home.iterchildren():
                        pl_data = td.xpath('./table/tr')
                        if len(pl_data):
                            pl = pl_data[0].xpath('./td/font')
                            if pl[0].text.isdigit():
                                plNum = pl[0].text
                                plName = pl[0].get('title').split('-')[1]
                                plPos = pl[0].get('title').split('-')[0]
                                SaveEvtRO(EvtROFName, G.gameId, per, playNo, 'h', plNum, plName, plPos)
                except:
                    pass
