/**
 * Berlin Brown
 *
 * MySQL oriented database creation scripts
 * prefix = akita_
 *
 * 1. akita_category
 * 2. pages
 * 3. articles
 * 4. source_modules
 * 5. images
 * 6. tags
 * 7. links
 * 8. comments
 * 9. users
 * 10. color_schema
 * 11. styles
 */

/**
 *  A category contains main pages.  An example category might include
 * pages related to java development
 */
CREATE TABLE akita_category(
 obj_id			INT UNSIGNED NOT NULL AUTO_INCREMENT,
 name			VARCHAR(128) NOT NULL,
 title_html		VARCHAR(255),
 description	TEXT,
 date_added		DATETIME DEFAULT '0000-00-00 00:00:00',
 author_id		INT(11),
 status			VARCHAR(20),
 PRIMARY KEY	(obj_id)
);

CREATE TABLE  akita_pages(
 obj_id			INT UNSIGNED NOT NULL AUTO_INCREMENT,
 category_id	INT(11) UNSIGNED,
 name			VARCHAR(128),
 title_html		VARCHAR(255),
 description	TEXT,
 date_added		DATETIME DEFAULT '0000-00-00 00:00:00',
 author_id		INT(11),
 status			VARCHAR(20),
 PRIMARY KEY	(obj_id)
);

/** bug = category_id should be page_id */
CREATE TABLE  akita_articles(
 obj_id			INT UNSIGNED NOT NULL AUTO_INCREMENT,
 page_id		INT(11) UNSIGNED NOT NULL,
 name			VARCHAR(128),
 title_html		VARCHAR(255),
 description	TEXT,
 date_added		DATETIME DEFAULT '0000-00-00 00:00:00',
 author_id		INT(11),
 status			VARCHAR(20),
 body_text		TEXT,
 size_bytes		int(11),
 PRIMARY KEY	(obj_id)
);

CREATE TABLE  akita_source_modules(
 obj_id			INT UNSIGNED NOT NULL AUTO_INCREMENT,
 article_id		INT(11) UNSIGNED,
 name			VARCHAR(128),
 description	TEXT,
 date_added		DATETIME DEFAULT '0000-00-00 00:00:00',
 lines_source	INT(11),
 body			TEXT,
 PRIMARY KEY	(obj_id)
);

CREATE TABLE  akita_images(
 obj_id 		INT UNSIGNED NOT NULL AUTO_INCREMENT,
 filename		VARCHAR(255),
 article_id		INT(11) UNSIGNED,
 type			VARCHAR(20),
 PRIMARY KEY	(obj_id)
);

CREATE TABLE  akita_tags(
 obj_id 		INT UNSIGNED NOT NULL AUTO_INCREMENT,
 article_id		INT(11) UNSIGNED,
 value			VARCHAR(40),
 type			VARCHAR(20),
 PRIMARY KEY	(obj_id)
);

CREATE TABLE  akita_links(
 obj_id			INT UNSIGNED NOT NULL AUTO_INCREMENT,
 article_id		INT(11),
 url			TEXT,
 link_descr		VARCHAR(255),
 type			VARCHAR(20),
 PRIMARY KEY	(obj_id)
);

CREATE TABLE  akita_comments(
 obj_id			INT UNSIGNED NOT NULL AUTO_INCREMENT,
 article_id		INT(11) NOT NULL,
 date_added		DATETIME DEFAULT '0000-00-00 00:00:00',
 body			TEXT,
 size_bytes		INT(11),
 author_id		INT(11),
 PRIMARY KEY	(obj_id)
);

CREATE TABLE  akita_users(
 obj_id			INT UNSIGNED NOT NULL AUTO_INCREMENT,
 date_added		DATETIME DEFAULT '0000-00-00 00:00:00' NOT NULL,
 user_name		VARCHAR(255) NOT NULL,
 email			VARCHAR(255),
 profile		TEXT,
 url			TEXT,
 karma			INT(11),
 UNIQUE			user_name (user_name),
 PRIMARY KEY	(obj_id)
);

/**
 * A schema will have multiple styles, styles
 */
CREATE TABLE  akita_color_schema(
 obj_id			INT UNSIGNED NOT NULL AUTO_INCREMENT,
 name			VARCHAR(255) NOT NULL,
 type			VARCHAR(40),
 PRIMARY KEY	(obj_id)
);

/**
 * A schema will have many styles
 */
CREATE TABLE  akita_styles(
 obj_id			INT UNSIGNED NOT NULL AUTO_INCREMENT,
 schema_id		INT(11) NOT NULL,
 style_name		VARCHAR(255) NOT NULL,
 type			VARCHAR(40),
 value			TEXT NOT NULL,
 PRIMARY KEY	(obj_id)
);

commit;
show tables;
