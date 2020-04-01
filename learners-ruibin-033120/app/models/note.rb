class Note < ActiveRecord::Base
  validates :title, presence: true, length: { minimum: 2 }
  validates :user, presence: true
  validates :body, presence: true, length: { minimum: 5 }

  belongs_to :user
  belongs_to :subject
end
