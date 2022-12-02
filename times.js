const fs = require('fs')
const { exec } = require('child_process')

const y = +process.argv[2] || new Date().getFullYear()

const aoc_sess = fs.readFileSync('session.txt', {encoding:'utf8', flag:'r'}).trim()
const aoc_addr = 'https://adventofcode.com/'+y+'/leaderboard/private/view/983136.json'
const aoc_comm = 'curl -s --cookie "session='+aoc_sess+'" '+aoc_addr

exec (aoc_comm, (error, stdout, stderr) => {
   let r = {}
   if (error) {
      console.error(error)
      return run(':(')
   }
   if (stderr) {
      console.error(stderr)
      return run(':(')
   }
   try {
      r = JSON.parse(stdout)
   } catch (err) {
      r = {}
   }
   return run(r)
})

function format (t) {

   const scs = 1
   const mns = 60*scs
   const hrs = 60*mns
   const dys = 24*hrs

   let ndys = Math.floor(t / dys)
   t -= ndys * dys
   let nhrs = Math.floor(t / hrs)
   t -= nhrs * hrs
   let nmns = Math.floor(t / mns)
   t -= nmns * mns
   let nscs = Math.floor(t / scs)
   t -= nscs * scs

   if (t) throw new Error ('illegal '+t+' seconds remainder')

   let ddys =
      ndys ? (ndys + '+') :
      ''
   let dhrs =
      ndys ? (nhrs + ':').padStart(3,'0') :
      nhrs ? (nhrs + ':') :
      ''
   let dmns =
      ndys ? (nmns + '\'').padStart(3,'0') :
      nhrs ? (nmns + '\'').padStart(3,'0') :
      nmns ? (nmns + '\'') :
      '\''
   let dscs = (nscs+'').padStart(2,'0')

   return ndys > 30 ? ndys+'+' : ddys + dhrs + dmns + dscs

}

function run (data) {

   let $ = Object.values
   let members = []
   let records = {}

   for (let member of $(data.members)) {
      members.push(member.name)
      for (let day in member.completion_day_level) {

         records[day] = records[day] || {}
         records[day][1] = records[day][1] || {}
         records[day][2] = records[day][2] || {}
         records[day]['delta'] = records[day]['delta'] || {}

         let p1 = $(member.completion_day_level[day])[0] || null
         let p2 = $(member.completion_day_level[day])[1] || null

         let release = +new Date(day+' Dec '+y) / 1000 //+ (5*60*60) // aoc starts at 5:00'00 UTC

         if (p1) {
            let t = p1.get_star_ts - release
            records[day][1][member.name] = format(t)
         }
         if (p2) {
            let t = p2.get_star_ts - release
            records[day][2][member.name] = format(t)
         }
         if (p1 && p2) {
            let t = p2.get_star_ts - p1.get_star_ts
            records[day]['delta'][member.name] = format(t)
         }
      }
   }

   let colorcodes = [31,32,33,34,35,36,91,92,93,94,95,96]
   let k = colorcodes.map( c => '\x1b['+c+'m' )
   let nok = '\x1b[0m'

   let print = ''

   const mgn = 1

   let header = ''

   header += '\n' + ''.padStart(2,' ')

   let ki = 0
   for (let member of members) {
      header += ''.padStart(mgn,' ')
      header += k[ki]
      header += member.substring(0,6).padStart(11,' ')
      header += nok
      ki ++
   }

   header += '\n'

   print += header

   for (let day in records) {

      print += '\n' + day.padStart(2,' ')
      for (let member of members) {
         print += ''.padStart(mgn,' ')
         print += (records[day] && records[day][1] && records[day][1][member] || '').padStart(11,' ')
      }

      print += '\n' + ''.padStart(2,' ')
      for (let member of members) {
         print += ''.padStart(mgn,' ')
         print += (records[day] && records[day][2] && records[day][2][member] || '').padStart(11,' ')
      }

      print += '\n' + ''.padStart(2,' ')
      let ki = 0
      for (let member of members) {
         print += ''.padStart(mgn,' ')
         print += k[ki]
         print += (records[day] && records[day]['delta'] && records[day]['delta'][member] || '').padStart(11,' ')
         print += nok
         ki ++
      }

      print += '\n'
      print += ' '
   }

   print += header

   console.log(print)

}

