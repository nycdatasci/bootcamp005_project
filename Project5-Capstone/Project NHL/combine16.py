import pandas as pd
import math
import codecs

if __name__ == '__main__':

    plays = pd.read_csv('events.csv', encoding='utf-8')
    playsRO = pd.read_csv('eventRO.csv', encoding='utf-8')

    RevColumns = ['gameId','period', 'playNo','evt','desc','time']

    for i in range(1,7):
        RevColumns = RevColumns + ['homeNa' + str(i), 'homeNo' + str(i), 'homePos' + str(i)]
    for i in range(1,7):
        RevColumns = RevColumns + ['awayNa' + str(i), 'awayNo' + str(i), 'awayPos' + str(i)]

    file = codecs.open('NHLData2016.csv', "w", "utf-8")
    file.write(','.join(RevColumns) + '\n')

    for N, E in plays.iterrows():
        period = E.per
        playNo = E.playNo
        evt = E.event
        desc = E.desc
        time = E.time

        Rec = [str(E.gameId), str(period), str(playNo), evt, '\"' + desc + '\"', str(time)]

        roster = playsRO.loc[(playsRO['gameId']==E['gameId']) & (playsRO[' per']==period) & (playsRO[' playNo']==playNo)]
        homeRecs = roster[roster[' ha'] == 'h']
        awayRecs = roster[roster[' ha'] == 'a']
        for i in range(0,6):
            if i < len(homeRecs):
                currRec = homeRecs.iloc[i]
                Rec = Rec + ['\"' + currRec[' plNa'] + '\"', str(currRec[' plNo']), currRec[' PlPo']]
            else:
                Rec = Rec + ['','','']

        for i in range(0,6):
            if i < len(awayRecs):
                currRec = awayRecs.iloc[i]
                Rec = Rec + ['\"' + currRec[' plNa'] + '\"', str(currRec[' plNo']), currRec[' PlPo']]
            else:
                Rec = Rec + ['','','']
        file.write(','.join(Rec) + '\n')
    file.close()

