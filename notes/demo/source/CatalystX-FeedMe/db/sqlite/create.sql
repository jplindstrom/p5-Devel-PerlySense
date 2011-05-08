




create table feed (
	feed_id integer primary key,

	name varchar(256) not null,

    create_time datetime not null default CURRENT_TIMESTAMP
);
create index feed_name_idx on feed (name);


insert into feed (name) values ('default');
insert into feed (name) values ('other');




create table feed_item (
	feed_item_id integer primary key,

    feed_id integer references url,

    title varchar(255) null,
    body text null,
    url varchar(255) null,

    create_time datetime not null default CURRENT_TIMESTAMP
);
create index feed_item_feed_create_time_idx on feed_item (feed_id, create_time);

insert into feed_item (feed_id, title, body) values (1, "First Item", "Some text: first");
insert into feed_item (feed_id, title, body) values (1, "Item Two", "Some text: second thing we write in this 'blog'");
insert into feed_item (feed_id, title, body) values (1, "Troisieme", "This is news");
    


--END
