select * from hr.employees
where first_name = 'David';

select * from hr.employees
where job_id = 'FI_ACCOUNT';

select first_name, last_name, salary, department_id from hr.employees
where department_id = 50 and salary > 4000;

select * from hr.employees
where department_id = 20 or department_id = 30;

select * from hr.employees
where first_name like '_a%a';

select * from hr.employees
where (department_id = 50 or department_id = 80) and commission_pct is not null
order by 4;

select * from hr.employees
where first_name like '%n%n' or first_name like '%nn%';

select * from hr.employees
where first_name like '_____%'
order by department_id DESC nulls last;

select * from hr.employees
where (salary between 3000 and 7000) and (commission_pct is null) and job_id in ('PU_CLERK',
'ST_MAN', 'ST_CLERK');

#w imieniu znajduje się '%'
select * from hr.employees
where first_name like '%\%%' ESCAPE '\';

select job_id, first_name, salary from hr.employees
where employee_id >= 120 and job_id != 'IT_PROG'
order by job_id, first_name DESC;