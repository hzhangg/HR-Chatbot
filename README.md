# HR-Chatbot
This repository is for our Chatbot project for CPSC312

Wiki Link: https://wiki.ubc.ca/Course:CPSC312-2021/Chatbot

To try our app,
1. Boot up jobScreener.pl 
2. Run "main."

## Description
Our HR-Chatbot lets users query against our database of encoded job postings. Users can create their own resume and modify it on the fly for further interactions in the session.

Features Include:
- A pleasant text formatted UI
- Search Job Postings according to optional criterion
- Lookup definitions of terms you may not know
- Create, update, view, and save resumes
- Measure your Qualifications against jobs
- Receive insight on your missing skills and their relevancy in jobs


Our program has quality of life features such as:
- Input validation (ex. number, wrong input)
- Input processing (ex. no need to sumbit answers in "" or with period)
- Skipping inputs (ex. type 'x' to skip the search criteria)


## Searching Jobs
User's may search for job postings based on:
- Location
- Industry 
- Position
- Minimum expected Salary
- Whether it is Fulltime
- Whether it is Remote

All jobs that satisfy your interests will have their details printed.

In the case none match the criterion

**No Jobs Matching Criteria**
   
Otherwise, a typical job may looks as follows

**Job ID#:**    1

**Location**    Vancouver

**Industry**    Computer Science

**Position**    Software Engineer

**Salary**      $40/hr

**Fulltime?**   Yes

**Remote?**     Yes

**Deadline**   1/31/2022

**Languages   Qualifications**
- English

**Programs    Qualifications**
- Microsoft Excel

**Education   Qualification**  
- Undergraduate

**Experience  Qualifications**
- C, 4 Years
- Python, 5 Years

## Resume Management
Users may create, view, and adjust their resume throughout the course of the program. 

We have robust validation to ensure:
- Duplicate or redundant facts are not asserted into your resume
- Values are updated properly
- Only 1 resume per person
- Only resumes that exist may be updated/viewed

Users also may save their resume:
- You may specify the file to save to 
- You may specify which resume to save 

## Qualifications Checking
Curious users may pit their resume against our database of job postings.

Each qualification property will use its own measurement function to determine if the skill is satified or not

Examples:
- "Masters" level of education will satisfy any "Masters", "Undergraduate", or "High School" requirement
- 4 years of Python experience will satify any requirement on Python experience of 4 Years of less

Users will receive a breakdown of how qualified they are for each category along with an overall score.


We also have a feature for users to request insight of what skills to focus on and allow judgement based on their importance.
- We compile your missing qualifications alphabetically
- We determine how many jobs require the skill

Knowing this, users are able to judge which skills they should priortize to become a better candidate. 

This was implemented with a Dictionary structure recreated through Prolog.





   
