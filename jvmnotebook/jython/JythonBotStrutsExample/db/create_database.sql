--
-- Create the database (botlist_)
--
create database botlist_development;
create database botlist_test;
create database botlist_production;

grant all on botlist_development.* to 'spirituser'@'localhost' identified by 'PASSWORD';
grant all on botlist_test.* to 'spirituser'@'localhost' identified by 'PASSWORD';
grant all on botlist_production.* to 'spirituser'@'localhost' identified by 'PASSWORD';

grant all on botlist_development.* to 'spirituser'@'*' identified by 'PASSWORD';
grant all on botlist_test.* to 'spirituser'@'*' identified by 'PASSWORD';
grant all on botlist_production.* to 'spirituser'@'*' identified by 'PASSWORD';

-- End of the File

