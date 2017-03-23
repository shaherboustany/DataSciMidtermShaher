OSHA Workplace Safety
README
Updated March 2006

The OSHA Workplace Safety database consists of all federal inspections at companies in the U.S. and its territorie between 1972 and 2005. OSHA provided the data to NICAR in March 2006.

The data ranges from January 1972 into February 2006. It includes a great deal of detail of each inspection during that time, including violations, accidents and penalities. For more detail or to verify facts, you can obtain copies of the actual reports.

TABLES

We provide these data tables with information about OSHA inspections:
	
	Osha.dbf - main table with company name, address, date of inspection, etc. 
		If you get the entire country, there's a number after the 
		word OSHA, since it's too big to put on one CD.
	Viol.dbf - violations from each inspection. If you get the entire 
		country, there's a number after the word VIOL, since it's 
		too big to put on one CD.
	Accid.dbf - details about accident victims
	Hazsub.dbf - hazardous substances involved
	Debt.dbf - status of debt
	History.dbf - outlines a history of any changes in penalty
	Admpay.dbf - a record of collecting administrative fees or penalties
	Prog.dbf - special programs the inspection might be involved in
	Relact.dbf - whether the inspection is related to another inspection or other action
	Optinfo.dbf - optional information
	
A listing of states, by file (if you've received the entire U.S.): 

OSHA1, VIOL1 -- AK,AL,AR,AS,AZ,CA,CO,CT,CZ,DC,DE,FL,FN,GA,GU,HI,IA,ID     
OSHA2, VIOL2 -- IN,JQ,KS,KY,LA,MA,MD,ME,MI,MN,MO,MP,MQ,MS,MT,NC,ND,NE,NH
OSHA3, VIOL3 -- NJ,NM,NV,NY,OH,OK,OR,PA,PI       
OSHA4, VIOL4 -- PR,RI,SC,SD,TN,TX,UT,VA,VI,VT,WA,WI,WV,WY,IL 

All other files include all states

If you received data on CD for the entire U.S., the data will be divided as follows:  

OSHA1 -- OSHA1, ACCID, plus readme, layouts, lookups and other OSHA documents
OSHA2 -- OSHA2, PROG, ADMPAY
OSHA3 -- OSHA3, RELACT
OSHA4 -- OSHA4, OPTINFO
OSHA5 -- VIOL1, DEBT, HAZSUB
OSHA6 -- VIOL2
OSHA7 -- VIOL3
OSHA8 -- VIOL4
OSHA9 -- HISTORY

Beginning in 2006, NICAR can also deliver the data files on DVD containing the same OSHA folders listed above.

The tables can be joined using the ACTIVITYNO field. You may notice that some numbers in the ACTIVITYNO field appear more than once. This number is supposed to be a unique identifier per OSHA inspection. According to Bruce Beveridge, OSHA stores each inspection as one single record in their database. When they create files for public use, they are forced to split large records into several records. The problem occurs when an inspection has more violations than one record can contain. In this case, they will begin a new record with the same unique identifying number. All of the inspection information will be the same, what you will notice is different is the violation information included. 

You can find online OSHA regulations, the OSHA Act, and field inspection reference manual at the following Web site: http://www.osha.gov/comp-links.html. You can also search http://www.osha.gov/oshstats/index.html for more recent information on specific inspections.

Here are the tables and more information on what they contain:

*Accidents (ACCID.DBF): Names of those injured, identifies the task that was being performed when the accident occurred, indicates whether a hazardous substance contributed to the accident, lists which body parts were injured, and indicates the degree of  the injury -- for example, whether it was fatal.

*Hazardous Substances (HAZSUB.DBF): Describes accidents that involve hazardous substances but that do not involve people.

*OSHA (OSHA.DBF): Gives company sites, previous violations,  the number of workers employed at the site in the previous 12 months and other OSHA activity at the sites, such as inspections or complaints.

*Violations (VIOL.DBF): Details penalties, types of violations -- willful, repeat, serious, etc. -- and the number of employees exposed to the danger.

*Special Programs (PROG.DBF): A description of any special programs -- local or national -- related to the specific inspection. Also included is documentation to explain further.

*Related Activity (RELACT.DBF) - If the inspection is related to another inspection, then the ACTIVITYNO of that inspection is listed. Other types of actions that might be related include Accidents, Complaints or Referrals.

*Optional Information (OPTINFO.DBF) - Indicates additional information recorded for the inspection.

*Debt (DEBT.DBF) - Information about a debt, including interest, fees and the outcome of the debt.

*Event History (HISTORY.DBF) - The history of penalties or failure to abate, including the amount and reason the amount was changed

*Administrative or Pay (ADMPAY.DBF) - A record of payment of administrative fees, penalties or failure-to-abate charges.

LAYOUTS, LOOKUPS AND CODES

The text files (Accid.txt, Hazsub.txt, Osha.txt, Viol.txt, Prog.txt, Relact.txt, Optinfo.txt, Debt.txt, History.txt, Admpay.txt) are the layouts for the 10 tables. These files also contain codes for each table. Over the years, we've discovered a few missing definitions for codes; we suspect most of those are internal typos at OSHA offices.

The remaining files (in the lookups folder) contain additional coding information for all of the tables. These files were current for OSHA as of April 2005, but some haven't changed since at least 1995. Use the text files in the folder LAYOUTS to find which file goes with which table: 

	ACC.DBF -- Accident Victim Attribute Code Values     
	FDA.DBF -- Federal Agency Code Values
	HZS.DBF -- Hazardous Substance Code Values
	OCC.DBF -- Occupational Code Values
	RID.DBF -- OSHA Office (Report ID or RID) Codes
	SCC.DBF -- State-City, State-County Code Values
	SIC.DBF -- Standard Industrial Classification Codes
	NAICS.DBF -- North American Industry Classification System Codes
	NEPTABLE.DBF -- Codes and descriptions for national special programs
	LEPTABLE.DBF -- Codes and descriptions for local special programs
	OPTINFO.DOC -- Codes and descriptons for optional information
	STD.DBF -- OSHA Federal Part-Section Standards Codes 
		  	   
Some additional OSHA documents:
	osha2098.pdf -- general information about OSHA inspections, including contact information about OSHA offices.
	drvddoc.doc -- a 1999 document with a file description, similar to the text record layouts

WORDS OF CAUTION

1) This database is quite large. If you're looking at the entire country, be sure you have hardware and software that can manage the information.

	To import into Microsoft Access: 
	1) Copy the tables from the CD to your hard drive. 
	2) Open a blank database in Access, name it and save it. 
	3) Inside the new database, in the File menu select "Get external data." Then select "Import."
	4) An import wizard will ask you to locate the file(s). You will need to change the "file of type" to "Dbase IV". 
	5) Each table will need to be imported separately.

2) Examine penalty fields carefully. There are several different fields, each with a different meaning -- 

In the OSHA table:
	PENREMIT -- This is the sum of penalties the company has paid on all violations up to the time NICAR received the data. (Note: this amount may be lower than the amount in PENNCURRENT because it reflects only the amount OSHA has received, not the amount the agency expects to receive.)
	FTAREMIT -- this is the final amount in failure to abate penalties paid by the company. 
	TOTPENLTY -- This is the total current amount in penalties issued. (this amount may have been result of negotiations between company and OSHA.)
	TOTALFTA -- This is the total current amount in failure to abate penalties issued. (this amount could also have been negotiated)	

In the VIOLS table:
	PENINITIAL -- This is the initial amount of penalties issued after the violation is assessed, but before appeals and reductions.
	PENCURRENT -- This represents the total amount OSHA has collected or expects to collect. This does not mean that OSHA will actually collect this money. (Sometimes payment is not complete due bankruptcy or other reasons.) This amount can be different from the one originally assessed, (found in the PENINITIAL field), because it reflects any changes made to the total due to negotiations, reductions, etc.
	FTAPEN -- Amount of failure to abate penalties issued for that violation.

In the HISTORY table:
	FTAPEN -- FTA penalty at the time current event/activity applied to record
	HISTPEN -- Penalty at the time current activity/event applied to record
	
In the ADMPAY table:
	PENAMT -- Amount of payment received for assessed penalties		
	FTAAMT -- Amount of payment received for failure-to-abate charge


3) Information can change if an inspection hasn't been closed yet. Check for updated information at http://www.osha.gov/oshstats/index.html.

If you have further questions, please call NICAR at 573-884-7332 or 573-884-7711. A contact number for OSHA about the data: (202) 693-1700. It would be a good idea to contact a local OSHA office, too.


OSHA STORIES AND TIPSHEETS

The New York times won a 2004 Pulitzer Prize for public services with a series of stories investigating companies where workers have died due to the negligence of their employers. As the investigations revealed many of these companies did not adhere to the safety regulations set up by OSHA. These articles reveal that five officials from McWane Inc. one of the companies that was investigated were later indicted. The reporters describe OSHA as "a toothless tiger" that does not always take these companies to task. (IRE Resource Center story #20850)

The KC Star analyze the Occupational Safety and Health Administration inspection database for the Kansas City metropolitan area, reviewed public records and interviewed more than 100 people in order to determine how well OSHA protects workers. The 2005 story found that OSHA fines employees in workers' deaths less than it should and downgrades its most serious violations in workers' deaths, hurting workers who are trying to sue employees. OSHA is behind in its safety standards. (Story #22226)

At the 2002 San Francisco conference, Mary Fricker of the Santa Rosa Press Democrat, Diana Hembree of Consumer Health Interactive, and Natalya Shulyakovskaya of the Orange County Register detailed ideas for reporting on worker safety. The tipsheets from this panel offer relevant Web addresses, a list of federal, state and local sources for covering occupational health, as well as story ideas. The tipsheets are #1608 (Shulyakovskaya), #1757 (Hembree) and #1758 (Fricker).

A computer-assisted investigation by the Free Lance-Star (Fredericksburg, Va.) in 2001 revealed that state government agencies are among the most common violators of worker safety standards, but - unlike private entities - they never get fined. Within the story is a list of the top violators in the Fredericksburg area in the past 25 years. Among the top ten violators are the Virginia Department of Transportation, the City of Fredericksburg and the County of Stafford. Story #18656

A five-part Newsday investigation in 2001 found that New York has the nation's highest rate of immigrants killed in the workplace. The series also uncovered that government agencies frequently fail to investigate deaths, provide enforcement for violations and offer timely compensation for victims and their families. Story #18983
 
A computer-assisted investigation by Dateline NBC in 1995 revealed a shocking pattern of lax enforcement at the agency charged with protecting worker safety. Multiple examples show that OSHA is reducing fines 99% of the time, collecting only 33 cents on the dollar. The story of Patrick Hayes, killed in a corn silo, was chosen to highlight how OSHA can save on fines and cri

The Arizona Republic examined OSHA inspection records and took an in-depth look at the estate workers' compensation system. Using an OSHA database, the paper established the median fine assessed in connection with a workplace death in which a serious safety violation was found - $3,000. The project also included several sidebars that profiled workers that were killed on the job and showed the dramatic impact on their families.  Story #14944.

In 1995, the St. Louis Post-Dispatch analyzed the last 10 years of OSHA inspection records, and revealed that the agency does use high initial fines to get employers' attention, but then the fines are routinely reduced for those who cooperate and improve conditions. Story #12384.

In a November 1995 Uplink article, Jo Craven details how journalists have used the OSHA data, highlighting the advantages and pitfalls in the database.

At the 1992 Portland conference, Mike Casey and Russ Carollo of the Dayton Daily News outlined how to proceed in a workplace safety project. This tipsheet includes a source list, background on Occupational Safety and Health Administration, computer vs. non-computer reporting and public records. Also included is who to write for the OSHA database, institutions that have the OSHA tapes and can perform analysis, sample federal inspection list, reported occupational injury incidence rates by industry. Casey and Carollo's IRE award winning series on OSHA is Morgue File #8223 and their tipsheet is #31.
minal prosecutions, despite findings of deliberate violations of worker safety. (Aug. 1, 1995) See tipsheet entry #479 for more information on Hayes' case and FOIA problems encountered. Story #12472 

To order these stories and tipsheets, contact the IRE Resource Center by phone (573 882-3364) or email (rescntr@ire.org).




