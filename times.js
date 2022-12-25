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

   console.log(data)

}

