class Subject < ActiveRecord::Base
    # enables @subject.notes
    # This enables that, If you delete a subject, its associated notes will also be deleted.
    has_many :notes, dependent: :destroy

    # Validation happens at @subject.save (return t | f)
    validates :title, presence: true, length: { minimum: 2 }
end
