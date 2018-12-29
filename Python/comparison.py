from __future__ import division
import sys, os, numpy, subprocess, multiprocessing, time, gc

def ReadPrograms(filename):
    f = open(filename, 'r')
    lines = f.readlines()
    programs = {}
    ct = 0
    for line in lines:
        line = line.strip().rstrip('\r')
        pieces = line.split(';')
        if 'CODIGO' in pieces[0]:
            continue
        id_c = int(pieces[0])
        programs[id_c] = int(pieces[10]) + int(pieces[11]) + int(pieces[12]) + int(pieces[13])
    f.close()
    return programs

def ReadOfficialResults(infile):
    print 'Reading official results'
    lines = open(infile, 'r').readlines()
    results = {}
    for i in range(len(lines)):
        line = lines[i].rstrip('\n')
        if len(line)<187:
            continue

        if not line[0:7].isdigit():  # si el id del alumno no es un numero continuamos porque puede ser un encabezaod u otra cosa
            continue

        id = int(line[0:7])
        if id not in results:
            results[id] = {}

        for j in range(10):
            idx = 7 + j*18
            cc = int(line[idx:(idx+5)])
            mc = int(line[(idx+5):(idx+7)])
            pp = int(line[(idx+7):(idx+12)])
            od = int(line[(idx+12):(idx+17)])
            pa = int(line[(idx+17):(idx+18)])
            if mc not in [24,25]:
                continue
            results[id][j+1] = {'cc':cc, 'pp':pp, 'mc':mc, 'od':od}
    return results

def ReadAssignments(infile):
    data = {}
    f = open(infile, 'r')
    for line in f.readlines():
        line = line.strip().rstrip('\n')
        pieces = line.split(';')
        if 'id_alumno' in pieces[0]:
            continue

        id = int(pieces[0])
        if id not in data:
            data[id] = {}

        cc = int(pieces[1])
        pp = float(pieces[2])
        mc = int(pieces[3])
        od = int(pieces[4])
        pf = int(pieces[5])
        data[id][pf] = {'cc':cc, 'pp':pp, 'mc':mc, 'od':od}
    f.close()
    return data

def NumberAdmitted(dat):
    admitted = {}
    for id in dat:
        for p in dat[id]:
            if dat[id][p]['mc'] != 24:
                continue
            if dat[id][p]['cc'] not in admitted:
                admitted[dat[id][p]['cc']] = 0

            admitted[dat[id][p]['cc']]+=1
    return admitted

def AssignedPreference(dat):
    assigned = {}
    for id in dat:
        assigned[id] = -1 # meaning unassigned
        for p in dat[id]:
            if dat[id][p]['mc'] != 24:
                continue
            assigned[id] = p
            break
    return assigned

# extra vacancies, number of improvements, number of new assignments, number of programs with extra vacancies, maximum excess of capacity
# official results
# res = ReadOfficialResults('inputs_'+str(yr)+'/seleccion_reg.txt')
# nao = NumberAdmitted(res)


if __name__ == '__main__':
    S = 100
    stats = {}
    tbs = ['stb', 'mtb']
    for yr in [2014, 2015, 2016]:
        # programs
        print 'Reading programs'
        programs = ReadPrograms('inputs_'+str(yr)+'/carreras.csv')
        mechanism = ''
        if yr == 2014:
            mechanism = 'secuencial_university'
        elif yr == 2015:
            mechanism = 'secuencial_student'
        else:
            mechanism = 'unica_student'

        print '-----------------------------------------------------'
        print 'Processing year: ', yr, ' with mechanism ', mechanism
        print '-----------------------------------------------------'

        stats[yr] = {tb:{} for tb in tbs}
        for tb in tbs:
            dat = {}
            print '    Reading assignments -- ', tb
            for s in range(S+1):
                dat[s] = ReadAssignments('outputs_'+str(yr)+'/asignacion_obtenida_'+tb+'_reg_'+mechanism+'_s='+str(s)+'.csv')

            print '    Computing number of admitted students -- ', tb
            nad = {}
            for s in range(S+1):
                nad[s] = NumberAdmitted(dat[s])


            ######################
            ## Basic statistics
            ######################
            es, nes, mxes = 0,0,0
            for c in nad[0]:
                es += max(nad[0][c]-programs[c],0)
                if max(nad[0][c]-programs[c],0) > 0:
                    nes+=1
                if max(nad[0][c]-programs[c],0) > mxes:
                    mxes = max(nad[0][c]-programs[c],0)

            stats[yr][tb]['es'] = str(es)
            stats[yr][tb]['nes'] = str(nes)
            stats[yr][tb]['mxes'] = str(mxes)
            # print 'Total number of extra seats:', es
            # print 'Programs with extra seats:', nes
            # print 'Maximum excess of capacity:', mxes

            ######################
            ## Improvements and New assignments
            ######################
            print '    Computing preference of assignment -- ', tb
            ap = {}
            for s in range(S+1):
                ap[s] = AssignedPreference(dat[s])

            nim = {s: sum([1 for id in ap[s] if (ap[0][id] > 0) & (ap[0][id] < ap[s][id]) ]) for s in range(1,S+1)} # stores number of improvements for each simulation
            nna = {s: sum([1 for id in ap[s] if (ap[s][id] == -1) & (ap[0][id] > 0) ]) for s in range(1,S+1)} # stores number of new assigned for each simulation

            # print numpy.mean([nim[s] for s in nim]), numpy.std([nim[s] for s in nim]), numpy.mean([nna[s] for s in nna]), numpy.std([nna[s] for s in nna])
            stats[yr][tb]['nim_m'] = str(round(numpy.mean([nim[s] for s in nim]),1))
            stats[yr][tb]['nim_sd'] = str(round(numpy.std([nim[s] for s in nim]),1))
            stats[yr][tb]['nna_m'] = str(round(numpy.mean([nna[s] for s in nna]),1))
            stats[yr][tb]['nna_sd'] = str(round(numpy.std([nna[s] for s in nna]),1))

    ######################
    ## Write table
    ######################
    f = open('effect_flexible_quotas.tex', 'w')
    print >>f, r'\begin{table}'
    print >>f, r'\centering'
    print >>f, r'\caption{Impact of Flexible Quotas}'
    print >>f, r'\label{tab: impact flexible quotas}'
    print >>f, r'\begin{tabular}{lcccccc}'
    print >>f, r'\toprule'
    print >>f, r' & \multicolumn{3}{c}{STB} & \multicolumn{3}{c}{MTB} \\'
    print >>f, r' & 2014 & 2015 & 2016 & 2014 & 2015 & 2016 \\'
    print >>f, r'\midrule'
    print >>f, 'Extra Seats & '+ stats[2014]['stb']['es'] + '&' + stats[2015]['stb']['es'] + '&' + stats[2016]['stb']['es']  \
                         + '&' + stats[2014]['mtb']['es'] + '&' + stats[2015]['mtb']['es'] + '&' + stats[2016]['mtb']['es'] + r'\\'
    print >>f, 'Programs with flexible quotas & '+ stats[2014]['stb']['nes'] + '&' + stats[2015]['stb']['nes'] + '&' + stats[2016]['stb']['nes']  \
                         + '&' + stats[2014]['mtb']['nes'] + '&' + stats[2015]['mtb']['nes'] + '&' + stats[2016]['mtb']['nes'] + r'\\'
    print >>f, 'Maximum number of flexible quotas & '+ stats[2014]['stb']['mxes'] + '&' + stats[2015]['stb']['mxes'] + '&' + stats[2016]['stb']['mxes']  \
                         + '&' + stats[2014]['mtb']['mxes'] + '&' + stats[2015]['mtb']['mxes'] + '&' + stats[2016]['mtb']['mxes'] + r'\\'
    print >>f, r'\midrule'
    print >>f, 'Improvements & '+ stats[2014]['stb']['nim_m'] + '&' + stats[2015]['stb']['nim_m'] + '&' + stats[2016]['stb']['nim_m']  \
                         + '&' + stats[2014]['mtb']['nim_m'] + '&' + stats[2015]['mtb']['nim_m'] + '&' + stats[2016]['mtb']['nim_m'] + r'\\'
    print >>f, '            &('+ stats[2014]['stb']['nim_sd'] + ')&(' + stats[2015]['stb']['nim_sd'] + ')&(' + stats[2016]['stb']['nim_sd'] +')'  \
                         + '&('+ stats[2014]['mtb']['nim_sd'] + ')&(' + stats[2015]['mtb']['nim_sd'] + ')&(' + stats[2016]['mtb']['nim_sd'] +')'+ r'\\'
    print >>f, 'New Assigned & '+ stats[2014]['stb']['nna_m'] + '&' + stats[2015]['stb']['nna_m'] + '&' + stats[2016]['stb']['nna_m']  \
                         + '&' + stats[2014]['mtb']['nna_m'] + '&' + stats[2015]['mtb']['nna_m'] + '&' + stats[2016]['mtb']['nna_m'] + r'\\'
    print >>f, '            &('+ stats[2014]['stb']['nna_sd'] + ')&(' + stats[2015]['stb']['nna_sd'] + ')&(' + stats[2016]['stb']['nna_sd'] +')'  \
                         + '&('+ stats[2014]['mtb']['nna_sd'] + ')&(' + stats[2015]['mtb']['nna_sd'] + ')&(' + stats[2016]['mtb']['nna_sd'] +')'+ r'\\'
    print >>f, r'\bottomrule'
    print >>f, r'\end{tabular}'
    print >>f, r'\end{table}'
    f.close()
