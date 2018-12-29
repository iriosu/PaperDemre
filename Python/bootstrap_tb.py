from __future__ import division
import sys, os, numpy, subprocess, multiprocessing, time, gc

import Core_stb.Algoritmo, Core_stb.Entidades


numpy.random.seed(20181229)

# we start with reading the data. We use the files provided by DEMRE (from 2004 to 2017)
def ReadStudents(filename):
    stime = time.time()
    f = open(filename, 'r')
    lines = f.readlines()
    students = {}
    ct = 0
    print 'Storing information of students'
    for line in lines:
        if ct%10000 == 0:
            print '    Students processed:', ct
        ct+=1
        line = line.strip().rstrip('\r')
        pieces = line.split(';')
        if 'id_alumno' in pieces[0]:
            continue
        id_s = int(pieces[0])
        students[id_s] = {'pref':{}, 'bea': False}
        for i in range(10):
            idx = 1 + 4*i # this is the column in the csv where the information of applications begins
            if int(pieces[idx]) == 0:
                continue
            students[id_s]['pref'][i+1] = {'cc':0, 'mc':0, 'pp':0, 'pa':0}
            students[id_s]['pref'][i+1]['cc'] = int(pieces[idx])
            students[id_s]['pref'][i+1]['mc'] = int(pieces[idx+1])
            students[id_s]['pref'][i+1]['pp'] = int(pieces[idx+2])
            students[id_s]['pref'][i+1]['pa'] = int(pieces[idx+3])

        idx_bea = 1 + 4*10 # this is the column in the csv where the information of applications begins
        if int(pieces[idx_bea]) == 1:
            students[id_s]['bea'] = True
    f.close()
    print 'Elapsed time:', time.time()-stime
    return students

def ReadPrograms(filename):
    f = open(filename, 'r')
    lines = f.readlines()
    programs = {}
    ct = 0
    print 'Storing information of programs'
    for line in lines:
        line = line.strip().rstrip('\r')
        pieces = line.split(';')
        if 'CODIGO' in pieces[0]:
            continue
        id_c = int(pieces[0])
        programs[id_c] = Core_stb.Entidades.Carrera(id_c)
        programs[id_c].ponderado_minimo = int(pieces[8])*100
        programs[id_c].cutoff['reg'] = int(pieces[8])*100
        programs[id_c].cutoff['bea'] = int(pieces[8])*100
        programs[id_c].vacantes_reg = int(pieces[10]) + int(pieces[11]) + int(pieces[12]) + int(pieces[13])
        programs[id_c].vacantes_bea = int(pieces[21])
    f.close()
    return programs

def BootstrapIteration(data):
    dat_pg, dat_st, dat_tb, s, outdir, unified, alg, boo_stb = data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7]
    if s == 0:
        rn = numpy.zeros((len(dat_st), 11)) # random number to add to the score
    else:
        rn = dat_tb[s,]
    carreras = dat_pg
    alumnos = {}
    ct = 0
    for i in dat_st:
        alumnos[i] = Core_stb.Entidades.Alumno(i)
        dat_aux = dat_st[i]['pref']
        alumnos[i].pref = {j: dat_aux[j]['cc'] for j in dat_aux}
        # if single tie breaker,  0*int(boo_stb)+j*(1-int(boo_stb)) = 0 so it considers always the same number
        # if multiple tie breaker,  0*int(boo_stb)+j*(1-int(boo_stb)) = j so it considers different numbers for different preferences
        alumnos[i].puntpond = {j: (dat_aux[j]['pp']+rn[ct, 0*int(boo_stb)+j*(1-int(boo_stb))]) for j in dat_aux}
        alumnos[i].marca = {j: dat_aux[j]['mc'] for j in dat_aux}
        alumnos[i].puntano = {j: dat_aux[j]['pa'] for j in dat_aux}
        alumnos[i].bea = dat_st[i]['bea']
        ct+=1

    if unified == True:
        Core_stb.Algoritmo.EjecutarSeleccion(alumnos, carreras, ['reg','bea'], alg, outdir, s, boo_stb)
    else:
        Core_stb.Algoritmo.EjecutarSeleccion(alumnos, carreras, ['reg'], alg, outdir, s, boo_stb)
        Core_stb.Algoritmo.EjecutarSeleccion(alumnos, carreras, ['bea'], alg, outdir, s, boo_stb)

    # store outputs
    if boo_stb:
        tb_type = 'stb'
    else:
        tb_type = 'mtb'

    if unified == True:
        f = open(os.path.join(outdir, 'cutoffs_'+tb_type+'_unica_'+alg+'_s='+str(s)+'.csv'), 'w')
    else:
        f = open(os.path.join(outdir, 'cutoffs_'+tb_type+'_secuencial_'+alg+'_s='+str(s)+'.csv'), 'w')

    for c in sorted(carreras):
        f.write(str(c)+';'+str(carreras[c].cutoff['reg'])+';'+str(carreras[c].cutoff['bea'])+'\n')
    f.close()

if __name__ == '__main__':
    # 0. Parameters and inputs
    #### testing ####
    # num_cores = 1
    # S = 1 # bootstrap iterations
    # algorithms = ['student', 'university']
    # yrs = ['2014']
    # unis = [True]
    # tbrs = [True]

    #### real ####
    num_cores = 10
    S = 100 # bootstrap iterations
    algorithms = ['student', 'university']
    yrs = ['2014', '2015', '2016']
    unis = [True, False]
    tbrs = [True, False]

    for yr in yrs:
        outdir = 'outputs_'+yr
        if not os.path.exists(outdir):
            os.makedirs(outdir)

        # 1. read programs and vacancies
        programs = ReadPrograms('inputs_'+yr+'/carreras.csv')

        # 2. read applications
        students = ReadStudents('inputs_'+yr+'/postulaciones_post_preprocesamiento.txt')

        # 3. Random tie breakers, one for each student for each simulation
        tb = numpy.random.uniform(0,1,(S+1,len(students), 11))

        for algorithm in algorithms:
            if yr == '2014' and algorithm == 'student':
                continue
            if yr in ['2015', '2016'] and algorithm == 'university':
                continue
            for unified in unis:
                for boo_stb in tbrs:
                    print '---------------------------------------------------------'
                    print 'Running for: '+ algorithm + ' ' + yr + ', unified: '+str(unified) + ', STB: '+str(boo_stb)
                    print '---------------------------------------------------------'

                    # 4. Run simulations
                    # s=0 considers no random tie breaking, so it is equivalent to the Chilean case
                    BootstrapIteration((programs, students, tb, 0, outdir, unified, algorithm, boo_stb))

                    # s>0 considers random tie-breaker
                    indata = []
                    for sim in range(1,S+1):
                        indata.append((programs, students, tb, sim, outdir, unified, algorithm, boo_stb))

                    np = min(num_cores,multiprocessing.cpu_count())
                    # We execute our process for each replication with a pool
                    pool = multiprocessing.Pool(processes=min(np, len(indata)))
                    pool.map(BootstrapIteration, indata)
                    pool.close()
                    pool.join()
