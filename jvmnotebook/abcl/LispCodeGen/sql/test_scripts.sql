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
INSERT INTO akita_category VALUES(
	NULL,
	'Java Development',
	'Java Development',
  	'These pages are about java development',
 	'2005-07-17 03:03:00',
 	1,
 	'visible'
);

INSERT INTO   akita_pages VALUES(
					NULL,
 /*  category_id*/	1,
 /*  name*/			'java.dev.1',
 /*  title_html*/		'Java Development Page  - 1',
 /*  description*/	'Java Development Page  - 1',
 /*  date_added*/		'2005-07-17 03:03:00',
 /*  author_id*/		1,
 /*  status	*/		'visible'
);

INSERT INTO   akita_pages VALUES(
					NULL,
 /*  category_id*/	1,
 /*  name*/			'java.dev.2',
 /*  title_html*/		'Java Development Page  - 2',
 /*  description*/	'Java Development Page  - 2',
 /*  date_added*/		'2005-07-17 03:03:00',
 /*  author_id*/		1,
 /*  status*/			'visible'
);

/** bug = category_id should be page_id */
INSERT INTO   akita_articles VALUES(
					NULL,
 /*  category_id*/	1,
 /*  name*/			'Article About Java',
 /*  title_html*/		'Article About Java',
 /*  description*/	'This is about java, have fun',
 /*  date_added*/		'2005-07-17 03:03:00',
 /*  author_id*/		1,
 /*  status*/			'visible',
 /*  body_text*/		'This is about java, have fun',
 /*  size_bytes*/		32
);

INSERT INTO   akita_articles VALUES(
					NULL,
 /*  category_id*/	1,
 /*  name*/			'Article About Java - 2',
 /*  title_html*/		'Article About Java - 2',
 /*  description*/	'This is about java, have fun - 2',
 /*  date_added*/		'2005-07-17 03:03:00',
 /*  author_id*/		1,
 /*  status*/			'visible',
 /*  body_text*/		'This is about java, have fun',
 /*  size_bytes*/		32
);


INSERT INTO   akita_source_modules VALUES(
						NULL,
 /*  article_id*/		1,
 /*  name*/			'A.java',
 /*  description*/	'Java Class',
 /*  date_added*/		'2005-07-17 03:03:00',
 /*  lines_source*/	1,
 /*  body*/			'public class A { public static void main() {} }'
);

INSERT INTO   akita_tags VALUES(
						NULL,
 /*  article_id*/		1,
 /*  value*/			'dog',
 /*  type*/			'keyword'
);

INSERT INTO   akita_links VALUES(
						NULL,
 /*  article_id*/		1,
 /*  url*/			'http://www.dog.com',
 /*  link_descr*/		'Dog.com',
 /*  type*/			'article_link'
);

INSERT INTO   akita_comments VALUES(
						NULL,
 /*  article_id*/		1,
 /*  date_added*/		'2005-07-17 03:03:00',
 /*  body*/			'This sucks',
 /*  size_bytes*/	'32',
 /*  author_id*/		1
);

INSERT INTO   akita_users VALUES(
						NULL,
 /*  date_added*/		'2005-07-17 03:03:00',
 /*  user_name*/		'Berlin Brown',
 /*  email*/			'berlin.brown at gmail.com',
 /*  profile*/		'Berlin Brown is a profile',
 /*  url*/			'http://www.newspiritcompany.com',
 /*  karma*/			1
);

/**
 * A schema will have multiple styles, styles
 */
INSERT INTO   akita_color_schema VALUES(
						NULL,
 /*  name */			'new.schema',
 /* type */			'global'
);

/**
 * A schema will have many styles
 */
INSERT INTO   akita_styles VALUES(
					NULL,
 /*  schema_id */		1,
 /* style_name*/		'body.background.color',
 /*  type*/			'body.background.color',
 /*  value	*/		'background-color: green;'
);

commit;
select * from akita_articles;
