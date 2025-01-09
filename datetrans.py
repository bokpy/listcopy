#!/usr/bin/python3

NL_MAAND= {
    "Jan": "Jan",
    "Feb": "Feb",
    "Mar": "Mar",
    "Apr": "Apr",
    "May": "Mei",
    "Jun": "Jun",
    "Jul": "Jul",
    "Aug": "Aug",
    "Sep": "Sep",
    "Oct": "Okt",
    "Nov": "Nov",
    "Dec": "Dec"
}
NL_DAG={
    "Mon": "Ma",
    "Tue": "Di",
    "Wed": "Wo",
    "Thu": "Do",
    "Fri": "Vr",
    "Sat": "Za",
    "Sun": "Zo"
}

fy_days_long    = ['moandei','tiisdei','woansdei','tongersdei','freed','sneon','snein']
fy_months_short = ['Jan','Feb','Mrt','Apr','Mai','Jun','Jul','Aug','Sep','Okt','Nov','Des']
fy_months_long  = [	'jannewaris','febrewaris','maart','april','maaie','juny','july','augustus','septimber','oktober','novimber','desimber']
eng_days_short  = ['Mon','Tue','Wed','Thu','Fri','Sat','Sun']
eng_days_long   = [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
eng_months_long = [ "January","February","March","April","May","June","July","August","September","October","November","December"]

LANGUAGES={'nl':(NL_DAG,NL_MAAND),
           'fy':(FRIS_DAYS,FRIS_MONTHS),
           'eng':(ENG_DAYS,ENG_MONTHS)
           }

def main() -> None:
	pass

if __name__ == '__main__':
	main()
