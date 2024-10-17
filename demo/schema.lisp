(in-package :social-shopping)

(def-vertex merchant ()
  ((first-name :type string)
   (middle-name :type string)
   (last-name :type string)
   (email :type email)
   (company-name :type string)
   (description :type string)
   (store-url :type url)
   (status :type merchant-status)
   (status-reason :type string)
   (status-date :type timestamp)
   (phone :type phone)
   (mid :type string)
   (default-offer-commission :type integer)
   (date-added :type timestamp))
  :social-shopping)

(def-vertex location ()
  ((location-name :type string)
   status
   (phone :type phone)
   (address-1 :type string)
   (address-2 :type string)
   (city :type string)
   (state :type string)
   (zip :type zip-code)
   (latitude :type float)
   (longitude :type float)
   (date-added :type timestamp)
   (country :type country))
  :social-shopping)

(def-edge has-location ()
  ((date-added :type timestamp))
  :social-shopping)

(def-vertex address ()
  ((address-name :type string)
   (address-1 :type string)
   (address-2 :type string)
   (city :type string)
   (state :type state)
   (zip :type zip-code)
   (country :type country)
   (latitude :type float)
   (longitude :type float))
  :social-shopping)

(def-edge has-address ()
  ((date-added :type timestamp))
  :social-shopping)

(def-vertex customer ()
  ((first-name :type string)
   (middle-name :type string)
   (last-name :type string)
   (email :type email)
   (points :type integer)
   (phone-1 :type phone)
   (phone-2 :type phone)
   (gender :type gender)
   unique-identifier
   (username :type string)
   (curator-p :type boolean)
   (date-added :type timestamp)
   (visible-in-search :type boolean)
   (show-my-want-list :type boolean)
   (show-my-circles :type boolean))
  :social-shopping)

(def-vertex bio ()
  ((description :type string))
  :social-shopping)

(def-edge has-bio ()
  ()
  :social-shopping)

(def-edge has-sentiment-for ()
  ()
  :social-shopping)

(def-edge amplified-by ()
  ((start-timestamp :type timestamp)
   (end-timestamp :type timestamp))
  :social-shopping)

(def-edge has-sentiment-for ()
  ()
  :social-shopping)

(def-edge has-category-preference ()
  ()
  :social-shopping)

(def-vertex notification-preferences ()
  (emails
   amplification-alerts
   cashback-alerts)
  :social-shopping)

(def-edge has-notification-preferences ()
  ()
  :social-shopping)

(def-vertex credit-card ()
  (token
   gw-card-id
   (full-name :type string)
   (exp-month :type credit-card-month)
   (exp-year :type credit-card-year)
   last4)
  :social-shopping)

(def-edge has-credit-card ()
  ()
  :social-shopping)

(def-vertex product-taxon ()
  ((name :type string)
   (description :type string)
   (key :type string))
  :social-shopping)

(def-vertex collectable ()
  ()
  :social-shopping)

(def-vertex product (collectable)
  (sku
   (name :type string)
   (title :type string)
   (price :type integer)
   (currency :type currency)
   (upccode :type string)
   (description :type string)
   (category-words :type list)
   (gender :type gender)
   (manually-cataloged-p :type boolean)
   (linkurl :type url)
   (imageurl :type url)
   image-width
   image-height
   brand
   (last-updated :type timestamp)
   (short-description :type string)
   (long-description :type string))
  :social-shopping)

(def-edge has-product ()
  ()
  :social-shopping)

(def-edge has-taxon ()
  ()
  :social-shopping)

(def-vertex offer ()
  (title
   description
   image-width
   image-height
   (start-timestamp :type timestamp)
   (end-timestamp :type timestamp)
   (currency :type currency)
   (date-added :type timestamp)
   (max-connections :type integer)
   (minimum-purchase :type integer)
   (usage-count :type integer)
   (offer-type :type offer-type)
   (amount :type integer) ;; amount of cashback (mutually exclusive with percent)
   (percent :type integer) ;; percentage of cashback
   (commission :type integer) ;; commission as percentage of cash back
   (var-split :type integer) ;; the VAR's take
   (agent-split :type integer)) ;; the Agent's take
  :social-shopping)

(def-edge has-offer ()
  ()
  :social-shopping)

(def-edge has-active-offer ()
  ()
  :social-shopping)

(def-vertex redemption ()
  (amount
   commission
   card-token
   merchant-id
   source
   quantity
   currency
   date-time
   last4
   location-id
   order-identifier
   sku
   event-type)
  :social-shopping)

(def-edge has-redemption ()
  ((date-added :type timestamp))
  :social-shopping)

(def-edge redemption-of ()
  ()
  :social-shopping)

(def-vertex want-list ()
  ((collection-name :type string)
   (public-p :type boolean)
   (description :type string))
  :social-shopping)

(def-edge in-want-list ()
  ((date-added :type timestamp))
  :social-shopping)

(def-edge has-want-list ()
  ()
  :social-shopping)

;;; Collections
(def-vertex collection ()
  ((collection-name :type string)
   (date-added :type timestamp)
   (public-p :type boolean)
   (indexed-p :type boolean)
   (description :type string)
   (inactive-p :type boolean))
  :social-shopping)

(def-vertex membership ()
  ((date-added :type timestamp))
  :social-shopping)

(def-edge member-of () ;; for product memberships
  ()
  :social-shopping)

(def-edge is-showcased () ;; for hero shot product
  ()
  :social-shopping)

(def-edge has-membership () ;; for product memberships
  ()
  :social-shopping)

(def-edge added-by () ;; for product memberships
  ()
  :social-shopping)

(def-edge participant-in () ;; for collection pariticpants
  ()
  :social-shopping)

(def-edge created-by () ;; collection owner
  ()
  :social-shopping)

(def-edge moderated-by () ;; collection moderator
  ()
  :social-shopping)

(def-edge cloned-from () ;; copied from another collection
  ()
  :social-shopping)
;;;

(def-edge subscribed-to ()
  ((date-added :type timestamp))
  :social-shopping)

(def-edge following ()
  ()
  :social-shopping)

;;; Circles of social shopping friends
(def-vertex circle ()
  ((circle-name :type string)
   (public-p :type boolean)
   (description :type string))
  :social-shopping)

(def-edge has-circle ()
  ()
  :social-shopping)

(def-edge in-circle ()
  ((date-added :type timestamp))
  :social-shopping)

(def-edge can-view ()
  ((date-added :type timestamp))
  :social-shopping)

(def-vertex keyword-tag ()
  ((tag :type string))
  :social-shopping)

(def-edge has-keyword-tag ()
  ()
  :social-shopping)

(def-vertex hashtag ()
  ((tag :type string))
  :social-shopping)

(def-edge has-hashtag ()
  ()
  :social-shopping)

(def-edge used-hashtag ()
  ((date-added :type timestamp))
  :social-shopping)

(def-vertex bookmarklet (collectable)
  (url
   title
   description
   (gender :type gender)
   (date-added :type timestamp)
   (approved-p :type boolean))
  :social-shopping)

(def-vertex product-bookmarklet (bookmarklet)
  ((price :type integer))
  :social-shopping)

(def-edge bookmarked-by ()
  ()
  :social-shopping)

(def-edge has-bookmarklet ()
  ()
  :social-shopping)
