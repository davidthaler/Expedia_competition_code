create table ytrain(
  srch_id integer,
  prop_id integer,
  position integer,
  click_bool integer,
  gross_bookings_usd float,
  booking_bool integer
);

insert into ytrain 
  select srch_id, prop_id, position, click_bool, gross_bookings_usd, booking_bool
  from raw_train;