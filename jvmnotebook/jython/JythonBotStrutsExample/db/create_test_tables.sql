--
-- Berlin Brown
--
-- updated: 3/24/2006
--
-- connect to the
-- databatase
--
-- file: create_tables.sql
-- see insert_tables.sql

-- connect botlist_development;

CREATE TABLE user_links (
  id 				int(11) NOT NULL auto_increment,
  main_url			varchar(255) NOT NULL,
  url_title			varchar(128),
  url_description 	varchar(255),
  keywords			varchar(255),
  source			varchar(40),
  source_url		varchar(255),
  total_rating		int(11) DEFAULT 0,
  occurrence		int(11) DEFAULT 0,
  created_on		DATETIME NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (id)
);

-- End of file